module Utils
  ( scc
  , Matrix, setMatrix, setMatrixSafe, getMatrix, findMatrix
  , parMap, parMapM
  , allPairs)
  where

import Control.Concurrent (forkIO, newEmptyMVar, readMVar, putMVar, getNumCapabilities)
import Control.DeepSeq (force, NFData)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List.Extra (chunksOf, tails)

-- Adapted from curry-frontend, with code deduplication
scc :: Eq b => (a -> [b]) -- ^entities defined by node
            -> (a -> [b]) -- ^entities used by node
            -> [a]        -- ^list of nodes
            -> [[a]]      -- ^strongly connected components
scc bvs' fvs' = map (map node) . tsortWith bvs fvs (const []) (\x y z -> (x : concat z) : y)
                               . tsortWith fvs bvs id         (\x _ z -> x : z)
                               . zipWith wrap [0 ..]
  where
    wrap i n = Node i (bvs' n) (fvs' n) n
    tsortWith which others f g xs = snd (dfs xs Set.empty []) where
      dfs [] marks stack = (marks,stack)
      dfs (x : xs') marks stack
        | x `Set.member` marks = dfs xs' marks stack
        | otherwise = dfs xs' marks' (g x stack stack')
        where (marks',stack') = dfs (filter (any (`elem` which x) . others) xs)
                                    (x `Set.insert` marks) (f stack)

data Node a b = Node { key :: Int, bvs :: [b], fvs :: [b], node :: a }

instance Eq (Node a b) where
  n1 == n2 = key n1 == key n2

instance Ord (Node b a) where
  n1 `compare` n2 = key n1 `compare` key n2

type Matrix a = Vector (Vector a)

setMatrix :: Matrix a -> a -> Int -> Int -> Matrix a
setMatrix v a x y = case setMatrixSafe v a x y of
  Just m' -> m'
  Nothing -> error "Index out of bounds"

setMatrixSafe :: Matrix a -> a -> Int -> Int -> Maybe (Matrix a)
setMatrixSafe v a x y
  | x < 0 || y < 0 || x >= V.length v || y >= V.length (v V.! x) = Nothing
  | otherwise = Just $ v V.// [(x, (v V.! x) V.// [(y, a)])]

getMatrix :: Matrix a -> Int -> Int -> Maybe a
getMatrix v x y = (v V.!? x) >>= (V.!? y)

findMatrix :: (a -> Bool) -> Matrix a -> [(Int, Int)]
findMatrix p = concat . V.toList . V.imap (\i v -> (i,) <$> V.toList (V.findIndices p v))

parMap :: NFData b => (a -> b) -> [a] -> IO [b]
parMap f xs = do
  num <- getNumCapabilities
  vs <- mapM go (chunksOf (length xs `div` num) xs)
  concat <$> mapM readMVar vs
  where
    go x = do
      v <- newEmptyMVar
      _ <- forkIO $ putMVar v $! force (map f x)
      return v

parMapM :: NFData b => (a -> IO b) -> [a] -> IO [b]
parMapM f xs = do
  num <- getNumCapabilities
  vs <- mapM go (chunksOf (length xs `div` num) xs)
  concat <$> mapM readMVar vs
  where
    go x = do
      v <- newEmptyMVar
      _ <- forkIO $ do
        res <- mapM f x
        putMVar v $! force res
      return v

allPairs :: [b] -> [(b, b)]
allPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
