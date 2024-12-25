{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass  #-}
module Day17 where

import Control.DeepSeq (NFData)
import Control.Exception (Exception(displayException))
import Data.Bits (xor, shiftR)
import Data.List (intercalate)
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Parsec (Parsec, many1, sepBy1, parse)
import Text.Parsec.Char (newline, string, char, digit)

data Computer = C { aR, bR, cR :: Integer }
  deriving (NFData, Generic, Eq, Ord)

type Program = [Integer]
type Puzzle = (Program, Computer)
type Cache = Map Computer [Integer]

runProgram :: Program -> Computer -> [Integer]
runProgram p = go 0
  where
    go ip _ | ip >= toInteger (length p) = []
    go ip (C {..}) = case p !! fromInteger ip of
          0 -> go (ip + 2) (C (aR `div` (2^combo)) bR cR)        -- adv
          1 -> go (ip + 2) (C aR (bR `xor` lit) cR)              -- bxl
          2 -> go (ip + 2) (C aR (combo `mod` 8) cR)             -- bst
          3 -> go (if aR == 0 then ip + 2 else lit) (C aR bR cR) -- jnz
          4 -> go (ip + 2) (C aR (bR `xor` cR) cR)               -- bxc
          5 -> (combo `mod` 8) : go (ip + 2) (C aR bR cR)        -- out
          6 -> go (ip + 2) (C aR (aR `div` (2^combo)) cR)        -- bdv
          7 -> go (ip + 2) (C aR bR (aR `div` (2^combo)))        -- cdv
          _ -> error "INVALID"
        where
          lit = toInteger (p !! succ (fromInteger ip))
          combo = case p !! succ (fromInteger ip) of
            4 -> aR
            5 -> bR
            6 -> cR
            n -> toInteger n

-- for my input
runProgram' :: Int -> [Integer]
runProgram' 0 = []
runProgram' n =
  let l = n `mod` 8
      out = ((l `xor` 1 `xor` (n `shiftR` (l `xor` 1))) `xor` 4) `mod` 8
  in toInteger out : runProgram' (n `shiftR` 3)

runReverse :: [Integer] -> Integer -> [Integer]
runReverse []     a = [a]
runReverse (o:os) a =
  [ res | remainder <- [0..7]
  , (((remainder `xor` 1) `xor`
    ((8 * a + remainder) `div` (2 ^ (remainder `xor` 1))))
     `xor` 4)
     `mod` 8
    == o
  , res <- runReverse os (8 * a + remainder)
  ]

day17 :: IO ()
day17 = do
  input <- readFile "input/day17"
  case parse puzzle "" input of
    Left err -> putStrLn (displayException err)
    Right (p, c) -> do
      let normal = intercalate "," (map show (runProgram p c))
      let optimized = intercalate "," (map show (runProgram' (fromInteger (aR c))))
      if normal == optimized
        then do
          putStr "Part 1: "
          putStrLn optimized
          putStr "Part 2: "
          print (minimum $ runReverse (reverse p) 0)
        else error "optimized program is incorrect"

puzzle :: Parsec String () Puzzle
puzzle = flip (,) <$> computer <*> (newline *> program)
  where
    computer = C <$> register "A" <*> register "B" <*> register "C"
    register s = string "Register " *> string s *> string ": " *> number <* newline
    number = read <$> many1 digit
    program = string "Program: " *> number `sepBy1` char ','
