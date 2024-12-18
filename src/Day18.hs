module Day18 where

import Utils (Matrix, setMatrix)
import qualified Data.Vector as V
import Algorithm.Search (bfs)

simulate :: [(Int, Int)] -> Matrix Bool
simulate xs = simulate' xs
  (V.replicate (maxV + 1) (V.replicate (maxV + 1) False) )

simulate' :: [(Int, Int)] -> Matrix Bool -> Matrix Bool
simulate' [] m = m
simulate' ((x, y):xs) m = simulate' xs (setMatrix m True y x)

steps :: Matrix Bool -> Int
steps m = maybe 0 length $ bfs next final initial
  where
    next (x, y) = filter ok (neighbors (x, y))
    neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    ok (x, y) = x >= 0 && x <= maxV &&
                y >= 0 && y <= maxV &&
                not (m V.! y V.! x)
    final (x, y) = x == maxV && y == maxV
    initial = (0, 0)

maxV :: Int
maxV = 70

unreachableAt :: Int -> Int -> [(Int, Int)] -> Int
unreachableAt n m xs
  | n == m = n
  | steps (simulate (take mid xs)) == 0 = unreachableAt n mid xs
  | otherwise = unreachableAt (mid + 1) m xs
  where
      mid = (n + m) `div` 2

day18 :: IO ()
day18 = do
  let read' s = read ("(" ++ s ++ ")")
  input <- map read' . lines <$> readFile "input/day18"
  putStr "Part 1: "
  print (steps (simulate (take 1024 input)))
  let x = last (take (unreachableAt 1024 (length input) input) input)
  putStr "Part 2: "
  putStrLn (show (fst x) ++ "," ++ show (snd x))
