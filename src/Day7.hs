module Day7 where

import Data.List (isSuffixOf, partition)

canBeSolved ::  Bool -> (Int, [Int]) -> Bool
canBeSolved _    (_  , []  ) = False
canBeSolved _    (res, [x] ) = res == x
canBeSolved useC (res, x:xs) = or
  ([canBeSolved useC (res `div` x, xs)  | res `mod` x == 0] ++
   [canBeSolved useC (res - x, xs) | res > x] ++
   [canBeSolved useC (read (take (length r - length s) r), xs)
      | let (s, r) = (show x, show res), let (ls, lr) = (length s, length r)
      , useC, lr > ls, s `isSuffixOf` r])

day7 :: IO ()
day7 = do
  let tup (x:xs) = (read (init x), reverse (map read xs))
      tup _ = error "Malformed input"
  equations <- map (tup . words) . lines <$> readFile "input/day7"
  putStr "Part 1: "
  let (solvable, unsolvable) = partition (canBeSolved False) equations
  let res = sum $ map fst solvable
  print res
  putStr "Part 2: "
  print (res + sum (map fst $ filter (canBeSolved True) unsolvable))
