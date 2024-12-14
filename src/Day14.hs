{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE DeriveAnyClass #-}
module Day14 where

import Codec.Picture (PixelRGB8(..), generateImage, writePng)
import Control.DeepSeq (NFData)
import Control.Exception (displayException)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.Directory.Extra (createDirectoryIfMissing)
import Text.Parsec (Parsec, parse, endBy1, optionMaybe, many1)
import Text.Parsec.Char (string, char, newline, digit)

import Utils (parMap)

data Robot = R { x, y, vX, vY :: Int }
  deriving (NFData, Generic, Show)

simulate :: Int -> Int -> Int -> Robot -> Robot
simulate mX mY n r@(R {..}) =
  r { x = (x + n * vX) `mod'` mX
    , y = (y + n * vY) `mod'` mY
    }
  where mod' a b = ((a `mod` b) + b) `mod` b

addQuadrant :: Int -> Int -> Robot
            -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addQuadrant mX mY R {..} (q1, q2, q3, q4) =
  if | x < midX && y < midY -> (q1 + 1, q2, q3, q4)
     | x < midX && y > midY -> (q1, q2 + 1, q3, q4)
     | x > midX && y < midY -> (q1, q2, q3 + 1, q4)
     | x > midX && y > midY -> (q1, q2, q3, q4 + 1)
     | otherwise -> (q1, q2, q3, q4)  -- Discard if exactly on the midpoint
  where (midX, midY) = (mX `div` 2, mY `div` 2)

day14 :: IO ()
day14 = do
  input <- readFile "input/day14"
  case parse (robot `endBy1` newline) "" input of
    Left err -> putStrLn (displayException err)
    Right rs -> do
      rs' <- parMap (simulate 101 103 100) rs
      let (q1, q2, q3, q4) = foldr (addQuadrant 101 103) (0,0,0,0) rs'
      putStr "Part 1: "
      print (q1 * q2 * q3 * q4)
      let steps = 8179
      rs'' <- parMap (simulate 101 103 steps) rs
      printImage rs''
      putStr "Part 2: "
      print steps
      putStrLn "Image generated as \"images/day14.png\""

printImage :: [Robot] -> IO ()
printImage rs = do
  let green = PixelRGB8 0 100 0
  let white = PixelRGB8 255 255 255
  let m = foldr f Map.empty rs
      f R {..} = Map.insertWith (Map.unionWith (+)) x (Map.singleton y (1 :: Int))
  createDirectoryIfMissing True "images"
  let pic = generateImage (\x y -> maybe white (const green)
                                    (Map.lookup x m >>= Map.lookup y)) 101 103
  writePng "images/day14.png" pic

robot :: Parsec String () Robot
robot = R <$>
  (string "p=" *> number <* char ',') <*>
  (number <* char ' ')                <*>
  (string "v=" *> number <* char ',') <*>
  number
  where
    number = read' <$> optionMaybe (char '-') <*> many1 digit
    read' Nothing  xs = read xs
    read' (Just _) xs = negate (read xs)
