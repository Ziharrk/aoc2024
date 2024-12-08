module Day8 where

import Data.List (unfoldr)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import qualified Data.Vector as V

import Utils (Matrix, setMatrixSafe, allPairs)

getAntennas :: (((Int, Int), (Int, Int)) -> [(Int, Int)]) -> Matrix Char -> Matrix Char
getAntennas mkPeaks s = Map.foldr markAll s antennaFreqPlaces
  where
    zipped = concat $ V.toList $ V.imap (\x row -> V.toList $ V.imap (\y a -> (x, y, a)) row) s
    antennaFreqPlaces = foldr (\(x, y, c) m -> if c == '.' then m else Map.insertWith (++) c [(x, y)] m) Map.empty zipped
    markAll xs = flip (foldr markOne) (allPairs xs)
    markOne p = flip (foldr (\(x, y) m -> fromMaybe m (setMatrixSafe m '#' x y))) (mkPeaks p)

antinode1 :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
antinode1 ((x1, y1), (x2, y2)) = [(x1 + dx, y1 + dy), (x2 - dx, y2 - dy)]
  where (dx, dy) = (x1 - x2, y1 - y2)

antinode2 :: (Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)] 
antinode2 (maxX, maxY) ((x1, y1), (x2, y2)) = (x1, y1) : 
  unfoldr (generateNext (both negate d)) (x1, y1) ++ 
  unfoldr (generateNext d) (x1, y1)
  where 
    d = (x1 - x2, y1 - y2)
    generateNext (dx, dy) (x, y)
        | let (x', y') = (x + dx, y + dy),
          x' >= 0, y' >= 0, 
          x' < maxX, y' < maxY = Just ((x', y'), (x', y'))
        | otherwise            = Nothing

countPeaks :: Matrix Char -> Int 
countPeaks = V.sum . V.map (V.length . V.filter (== '#'))

day8 :: IO ()
day8 = do
  input <- lines <$> readFile "input/day8"
  let m = V.fromList $ map V.fromList input
  putStr "Part 1: "
  print (countPeaks (getAntennas antinode1 m))
  putStr "Part 2: "
  print (countPeaks (getAntennas (antinode2 (V.length m, V.length (m V.! 0))) m))
