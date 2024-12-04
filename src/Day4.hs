module Day4 where

import Data.List (isPrefixOf, transpose)
import Debug.Trace

searchXMAS :: [[Char]] -> Int
searchXMAS input = horizontal input + vertical + diagonal1 input + diagonal2
  where
    look [] = 0
    look ys@(_:rest) | "XMAS" `isPrefixOf` ys = look rest + 1
                     | "SAMX" `isPrefixOf` ys = look rest + 1
                     | otherwise              = look rest
    horizontal xs = sum (map look xs)
    vertical = horizontal (transpose input)
    diagonal1 xs = horizontal [get 0 i xs | i <- [0..maxI]] +
                   horizontal (tail [get i 0 xs | i <- [0..maxI]])
    get :: Int -> Int -> [[Char]] -> [Char]
    get xS yS xs = [e | off <- [0..(maxI-max xS yS)]
                      , let (x,y) = (xS + off, yS + off)
                      , let e = xs !! x !! y]
    diagonal2 = diagonal1 (reverse input)
    maxI = length input - 1

searchX_MAS :: [[Char]] -> Int
searchX_MAS input = length $ filter check groups
  where
    groups = [get x y| x <- [0..maxI], y <- [0..maxI]]
    get x y = [input !! (x+offX) !! (y+offY) | offX <- [0..2], offY <- [0..2]]
    maxI = length input - 3
    check [a,_,b,_,c,_,d,_,e] = [a,c,e] `elem` ["MAS", "SAM"] && [b,c,d] `elem` ["MAS", "SAM"]
    check _ = undefined

day4 :: IO ()
day4 = do
  input <- lines <$> readFile "input/day4"
  putStr "Part 1: "
  print $ searchXMAS input
  putStr "Part 2: "
  print $ searchX_MAS input
