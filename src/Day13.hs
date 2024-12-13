{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Day13 where

import Control.Exception (displayException)
import Data.Maybe (catMaybes)
import Data.SBV ( GeneralizedCV (..), optimize, OptimizeStyle (..)
                , sInteger, literal, constrain, (.==), minimize)
import Data.SBV.Internals (SMTModel(..), SMTResult (..))
import Data.SBV.Dynamic (OptimizeResult (..), CV (..), CVal (..))
import Text.Parsec (Parsec, parse)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.Char (digit, string, newline)

import Utils (parMapM)

data Machine = M { aX, aY, bX, bY, prizeX, prizeY :: Integer }

fewestCoins :: Machine -> IO (Maybe Integer)
fewestCoins M {..} = do
  r <- optimize Lexicographic $ do
    (a,b) <- (,) <$> sInteger "a" <*> sInteger "b"
    constrain (literal aX * a +
               literal bX * b
               .== literal prizeX)
    constrain (literal aY * a +
               literal bY * b
               .== literal prizeY)
    minimize "coins" $ 3 * a + b
  case r of
    LexicographicResult (Satisfiable _ model)
      | Just (RegularCV (CV _ (CInteger i))) <-
        lookup "coins" model.modelObjectives
      -> return (Just i)
    _ -> return Nothing

modifyMachine :: Machine -> Machine
modifyMachine m@M {..} = m {
    prizeX = 10000000000000 + prizeX,
    prizeY = 10000000000000 + prizeY
  }

day13 :: IO ()
day13 = do
  input <- readFile "input/day13"
  case parse (machine `sepBy1` newline) "" input of
    Left err -> putStrLn (displayException err)
    Right ms -> do
      putStr "Part 1: "
      parMapM fewestCoins ms >>= printRes
      putStr "Part 2: "
      parMapM (fewestCoins . modifyMachine) ms >>= printRes
  where printRes = print . sum . catMaybes

machine :: Parsec String () Machine
machine = M <$>
  (string "Button A: X+" *> number)    <*>
  (string ", Y+" *> number <* newline) <*>
  (string "Button B: X+" *> number)    <*>
  (string ", Y+" *> number <* newline) <*>
  (string "Prize: X=" *> number)       <*>
  (string ", Y=" *> number <* newline)
  where
    number = read <$> many1 digit
