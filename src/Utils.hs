module Utils (scc) where

import qualified Data.Set as Set

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
