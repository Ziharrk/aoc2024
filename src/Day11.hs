{-# LANGUAGE LambdaCase #-}
module Day11 where

import Control.Monad.Trans.State (runState, gets, modify)
import qualified Data.Map as Map

applyRule :: Int -> [Int]
applyRule 0 = [1]
applyRule n | let ds = show n
            , let numD = length ds
            , even numD = (\(x, y) -> [read x, readZ y]) $
                          splitAt (numD `div` 2) ds
            | otherwise = [n * 2024]
  where
    readZ xs' = case dropWhile (=='0') xs' of
      [] -> 0
      xs -> read xs

type Cache = Map.Map (Int, Int) Int
simulate' :: Cache -> Int -> [Int] -> (Int, Cache)
simulate' cache n'' xs'' = runState (sim n'' xs'') cache
  where
    sim n' = fmap sum . mapM (simOne n')
    simOne 0 _ = return 1
    simOne n e = gets (Map.lookup (n, e)) >>= \case
        Nothing  -> do
          let res = applyRule e
          modify (Map.insert (1, e) (length res))
          res' <- sim (pred n) res
          modify (Map.insert (n, e) res')
          return res'
        Just res -> return res

day11 :: IO ()
day11 = do
  input <- map read . words <$> readFile "input/day11"
  putStr "Part 1: "
  let (n, c) = simulate' Map.empty 25 input
  print n
  putStr "Part 2: "
  print (fst $ simulate' c 75 input)
