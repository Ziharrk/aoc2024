module Day3 where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Data.Maybe (catMaybes)
import Text.Parsec (Parsec, parse, many, try)
import Text.Parsec.Combinator (many1, anyToken)
import Text.Parsec.Char (char, digit, string)

data Instruction = Mul Int Int
                 | Do
                 | Dont
  deriving Eq

eval :: Instruction -> Int
eval (Mul x y) = x * y
eval _ = 0

execute :: [Instruction] -> [Instruction]
execute []              = []
execute (Do       : xs) = execute xs
execute (Dont     : xs) = execute (dropWhile (/=Do) xs)
execute (x@Mul {} : xs) = x : execute xs

day3 :: IO ()
day3 = do
  input <- readFile "input/day3"
  case parse parseMemory "" input of
    Right parsed -> do
      putStr "Part 1: "
      print $ sum $ map eval parsed
      putStr "Part 2: "
      print $ sum $ map eval $ execute parsed
    Left err -> putStrLn (displayException err)

type Parser = Parsec String ()

parseMemory :: Parser [Instruction]
parseMemory = catMaybes <$>
  many (    Just    <$> (parseMul <?|> parseOther)
        <|> Nothing <$ anyToken)
  where
    parseMul =
      Mul <$> (string "mul(" *> integer)
          <*> (char ',' *> integer) <* char ')'
    integer = read <$> many1 digit
    parseOther =   Do   <$ string "do()"
              <?|> Dont <$ string "don't()"
    infix 0 <?|>
    p1 <?|> p2 = try p1 <|> p2
