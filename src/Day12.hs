module Day12 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Utils (Matrix)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

inBounds :: Matrix a -> (Int, Int) -> Bool
inBounds mat (x, y) = x >= 0 && y >= 0 && x < V.length mat && y < V.length (mat V.! 0)

dfs :: Matrix Char -> Char -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
dfs mat c start@(x, y) visited
  | not (inBounds mat start) || start `Set.member` visited || mat V.! x V.! y /= c
              = visited
  | otherwise = foldr (dfs mat c) (Set.insert start visited) (neighbors start)

segmentGarden :: Matrix Char -> [Set (Int, Int)]
segmentGarden mat = go Set.empty
  [(x, y) | x <- [0 .. V.length mat - 1], y <- [0 .. V.length (mat V.! 0) - 1]]
  where
    go _ [] = []
    go visited (p@(x,y):ps)
      | p `Set.member` visited = go visited ps
      | otherwise = let region = dfs mat (mat V.! x V.! y) p Set.empty
                    in region : go (Set.union visited region) ps

circumference :: Set (Int, Int) -> [(Int, Int)]
circumference region = foldr get [] (Set.toList region)
  where
    get c n = boundary c ++ n
    boundary = filter (not . (`Set.member` region)) . neighbors

sides :: Set.Set (Int, Int) -> Int
sides region = sum $ map (numCorners region) (Set.toList region)

numCorners :: Set (Int, Int) -> (Int, Int) -> Int
numCorners region (x, y) = length $ filter
  (all (\(member, dx, dy) -> (x + dx, y + dy) `Set.member` region == member))
  [ f c | let r = map (\(m, dx, dy) -> (m, -dy, dx))
        , c <- [[(False,1,0),(False,0,1)], [(True,1,0),(True,0,1),(False,1,1)]]
        , f <- [id, r, r . r, r . r . r]]  -- heh

day12 :: IO ()
day12 = do
  input <- V.fromList . map V.fromList . lines <$> readFile "input/day12"
  let segments = segmentGarden input
  putStr "Part 1: "
  print (sum $ map (\s -> Set.size s * length (circumference s)) segments)
  putStr "Part 2: "
  print (sum $ map (\s -> Set.size s * sides s) segments)
