module Day1 where

import Data.List ( sort )
import Data.Tuple.Extra ( both )

distance :: ([Int], [Int]) -> Int
distance (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) xs ys

similarity :: ([Int], [Int]) -> Int
similarity (xs, ys) = sum $ map
  (\x -> (x *) $ length $ takeWhile (==x) $ dropWhile (<x) ys) xs

day1 :: IO ()
day1 = do
  input <- readFile "input/day1"
  let parsed = both sort $ unzip $ map tuple $ lines input
  putStr "Part 1: "
  print (distance parsed)
  putStr "Part 2: "
  print (similarity parsed)

tuple :: String -> (Int, Int)
tuple xs =
  let (x, y) = span (/= ' ') xs
  in (read x, read y)
