{-# LANGUAGE LambdaCase #-}
module Day2 where

import Data.Maybe (isJust)
import Control.Applicative (Alternative(..))

numSafe :: [[Int]] -> Int
numSafe = length . filter (isJust . isSafe)

isSafe :: [Int] -> Maybe Ordering
isSafe [x,y] | let d = abs (x - y), d >= 1, d <= 3 = return (compare x y)
isSafe (x:y:xs) =
  isSafe (y:xs) >>= \case
    LT | x < y && y - x <= 3 -> return LT
    GT | x > y && x - y <= 3 -> return GT
    _                        -> Nothing
isSafe _ = empty

numSafeDampened :: [[Int]] -> Int
numSafeDampened = length . filter (any @[] (isJust . isSafe) . dampen)
  where
    dampen []     = return []
    dampen (x:xs) = (x:) <$> dampen xs <|> return xs

day2 :: IO ()
day2 = do
  input <- map (map read . words) . lines <$> readFile "input/day2"
  putStr "Part 1: "
  print $ numSafe input
  putStr "Part 2: "
  print $ numSafeDampened input
