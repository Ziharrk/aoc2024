module Day25 where

import Data.List.Extra (wordsBy, partition, transpose)

classify :: [[Char]] -> Bool
classify []    = error "empty schematic"
classify (x:_) = all (=='#') x

getProfile :: [[Char]] -> [Int]
getProfile = map (pred . length . takeWhile (=='#')) . transpose

fits :: [Int] -> [Int] -> Bool
fits xs = and . zipWith (\a -> (< 6) . (+a)) xs

day25 :: IO ()
day25 = do
  input <- lines <$> readFile "input/day25"
  let schematics = wordsBy null input
  let (locks, keys) = partition classify schematics
  let (lockProfiles, keyProfiles) = (map getProfile locks, map (getProfile . reverse) keys)
  putStr "Part 1: "
  print (length $ filter (uncurry fits) $ allCombinations lockProfiles keyProfiles)
  putStrLn "No Part 2 on day 25"

allCombinations :: [a] -> [b] -> [(a, b)]
allCombinations xs ys = [(x, y) | x <- xs, y <- ys]
