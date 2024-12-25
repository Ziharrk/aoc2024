module Day19 where

import Control.Exception (displayException)
import Control.Monad.Trans.State (evalState, get, modify, State)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (Parsec, choice, endBy1, sepBy1, many1, parse)
import Text.Parsec.Char (char, newline, string)

numPossible :: Ord a => [[a]] -> [a] -> State (Map [a] Int) Int
numPossible towels = go
  where
    go [] = return 1
    go xs = do
      cache <- get
      case Map.lookup xs cache of
        Just n  -> return n
        Nothing ->
          let fit = filter (`isPrefixOf` xs) towels
          in do
            res <- sum <$> mapM (\d -> go (drop (length d) xs)) fit
            modify (Map.insert xs res)
            return res

day19 :: IO ()
day19 = do
  input <- readFile "input/day19"
  case parse puzzle "" input of
    Left err -> putStrLn (displayException err)
    Right (ts, ds) -> do
      let nums = evalState (mapM (numPossible ts) ds) Map.empty
      putStr "Part 1: "
      print (length (filter (>0) nums))
      putStr "Part 2: "
      print (sum nums)

puzzle :: Parsec String () ([[Char]], [[Char]])
puzzle = (,) <$> towels <* newline <* newline <*> designs
  where
    towels = towel `sepBy1` string ", "
    towel = many1 color
    color = choice [char c | c <- "wubrg"]
    designs = design `endBy1` newline
    design = many1 color
