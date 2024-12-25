module Day20 where

import Algorithm.Search (bfs)
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Vector as V

import Day12 (neighbors)
import Utils (findMatrix, getMatrix, Matrix)

raceOn :: Matrix Char -> Maybe [(Int, Int)]
raceOn m = bfs step (==finish) initial
  where
    step p = filter valid (neighbors p)
    valid (x, y) = case getMatrix m x y of
      Just '#' -> False
      _        -> True
    finish = case findMatrix (== 'E') m of
      [p] -> p
      _   -> error "no end"
    initial = case findMatrix (== 'S') m of
      [p] -> p
      _   -> error "no start"

numCheats :: Int -> [(Int, Int)] -> Int
numCheats ps path = length possibleCheats
  where
    possibleCheats =
      [ ((ex, ey), (sx, sy)) | let pLen = length path - 1
           , si <- [0..pLen], let (sx, sy) = path !! si
           , dx <- [0..ps], dy <- [0..ps-dx]
           , dx + dy >= 2
           , (ex, ey) <- nubOrd [ (f sx dx, g sy dy) | f <- [(+), (-)], g <- [(+), (-)]]
           , Just ei <- return $ indexOf (ex, ey)
           , ei - si - dx - dy >= 100]
    indexOf e = Map.lookup e idxTable
    idxTable = Map.fromList $ zip path [0..]

day20 :: IO ()
day20 = do
  input <- V.fromList . map V.fromList . lines <$> readFile "input/day20"
  case raceOn input of
    Nothing -> error "no solution"
    Just path -> do
      let initial = case findMatrix (== 'S') input of
            [p] -> p
            _   -> error "no start"
      putStr "Part 1: "
      print (numCheats 2  (initial:path))
      putStr "Part 2: "
      print (numCheats 20 (initial:path))
