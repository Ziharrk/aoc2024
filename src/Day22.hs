module Day22 where

import Data.Bits (xor)
import Data.Map (Map)
import qualified Data.Map as Map

import Utils (parMap)

nextPRand :: Int -> Int
nextPRand n =
  let s1 = prune ((n * 64) `xor` n)
      s2 = prune ((s1 `div` 32) `xor` s1)
      prune = (`mod` 16777216)
  in prune ((s2 * 2048) `xor` s2)

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f a = scanr ($) a (replicate n f)

mkMoneyMap :: [Int] -> Map (Int, Int, Int, Int) Int
mkMoneyMap = go . derive . map (`mod` 10)
  where
    derive xs = zipWith (\n m -> (n - m, m)) xs (drop 1 xs)
    go ((a,_):xs@((b,_):(c,_):(d, v):_)) =
           Map.insert (a, b, c, d) v (go xs)
    go _ = Map.empty

day22 :: IO ()
day22 = do
  input <- map read . lines <$> readFile "input/day22"
  res <- map reverse <$> parMap (iterateN 2000 nextPRand) input
  putStr "Part 1: "
  print (sum (map last res))
  moneyMaps <- parMap mkMoneyMap res
  putStr "Part 2: "
  print (maximum (Map.unionsWith (+) moneyMaps))
