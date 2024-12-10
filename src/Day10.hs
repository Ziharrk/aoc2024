{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Day10 where

import Data.Array ((!))
import Data.Graph (graphFromEdges, reachable, Vertex, Graph)
import Data.Maybe (mapMaybe)

score :: Graph -> (Vertex -> Char) -> Vertex -> Int
score g f v = -- accumulator style, why not?
  let count '9' = succ
      count _   = id
  in foldr (count . f) 0 (reachable g v)

rating :: Graph -> (Vertex -> Char) -> Vertex -> Int
rating g f v = dfs v 0
  where
    -- fused with counting via chaining (+1) functions
    dfs v' | f v' == '9' = succ
           | otherwise   = foldr ((.) . dfs) id (g ! v')

mkGraph :: [[(Int, Int, Char)]] -> (Graph, _, _)
mkGraph input =
  let sX = length input
      sY = length (head input)
      getCellEdges (i, j, c) = (c, (i,j),
        [(x, y) | (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]
                , let x = i + dx, let y = j + dy
                , 0 <= x, x < sX, 0 <= y, y < sY
                , trd3 (input !! y !! x) == succ c])
      edges = concatMap (map getCellEdges) input
  in graphFromEdges edges

day10 :: IO ()
day10 = do
  input <- zipWith (\j s -> zipWith (, j,) [0 .. ] s) [0..] . lines <$> readFile "input/day10"
  let (g, fromVertex, toVertex) = mkGraph input
  let starts = concatMap (mapMaybe (\(x,y,_) -> toVertex (x, y)) .
                          filter ((=='0') . trd3)) input
  putStr "Part 1: "
  print $ sum $ map (score g (fst3 . fromVertex)) starts
  putStr "Part 2: "
  print $ sum $ map (rating g (fst3 . fromVertex)) starts

fst3 :: (a, b, c) -> a
fst3 (e, _, _) = e

trd3 :: (a, b, c) -> c
trd3 (_, _, e) = e
