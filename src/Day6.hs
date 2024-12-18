module Day6 where

import Data.List ((\\))
import qualified Data.Set as Set
import qualified Data.Vector as V

import Utils (Matrix, setMatrix, getMatrix, findMatrix, parMap)

data Dir = N | E | S | W
  deriving (Eq, Ord, Enum, Show)

fillPath :: Matrix Char -> (Dir, Int, Int) -> (Matrix Char, Bool)
fillPath = fillPath' Set.empty
  where
    fillPath' cache grid pos@(d, x, y)
      | Set.member pos cache = (grid, True)
      | otherwise = case nextPos (fromEnum d) of
          Just p  -> fillPath' (Set.insert pos cache) (setMatrix grid 'X' x y) p
          Nothing -> (setMatrix grid 'X' x y, False)
        where
          incPos d' = [(x-1, y), (x, y+1), (x+1, y), (x, y-1)] !! d'
          nextPos d' =
            let (x', y') = incPos d'
            in case getMatrix grid x' y' of
                 Just '#' -> nextPos ((d' + 1) `mod` 4)
                 Nothing  -> Nothing
                 _        -> Just (toEnum d', x', y')

tryIndex :: Matrix Char -> (Int, Int) -> (Int, Int) -> Bool
tryIndex grid (sX, sY) (x, y) =
  snd $ fillPath (setMatrix grid '#' x y) (N, sX, sY)

day6 :: IO ()
day6 = do
  input <- lines <$> readFile "input/day6"
  let matrix = V.fromList $ map V.fromList input
  case findMatrix (=='^') matrix of
    [(x, y)] -> do
      let (p, _) = fillPath matrix (N, x, y)
      let pathIdxs = findMatrix (== 'X') p \\ [(x, y)]
      putStr "Part 1: "
      print (length pathIdxs + 1)
      xs <- parMap (tryIndex matrix (x, y)) pathIdxs
      putStr "Part 2: "
      print (length $ filter id xs)
    _ -> error "No starting position found"
