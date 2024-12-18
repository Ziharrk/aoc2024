{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Day15 where

import Control.Arrow (second)
import Control.Exception (Exception(displayException))
import Data.Functor (($>))
import qualified Data.Vector as V
import Text.Parsec (Parsec, parse, char, newline, many1, endBy1, choice)

import Day6 (Dir(..))
import Utils (Matrix, findMatrix, setMatrix, getMatrix)

data Bathroom = B { bMap :: Matrix Field, robotMoves :: [Dir] }
data Field = Empty | Box | BoxL | BoxR | Robot | Border deriving Eq

simulate :: Matrix Field -> (Int, Int) -> [Dir] -> Matrix Field
simulate field _      []     = field
simulate field (x, y) (d:ds) =
  case moveInDir (x, y) Robot field of
    Nothing             -> simulate field (x, y) ds
    Just (field', pos') -> simulate (setMatrix field' Empty x y) pos' ds
  where
    withDir (x', y') = case d of
      N -> (x' - 1, y')
      E -> (x', y' + 1)
      S -> (x' + 1, y')
      W -> (x', y' - 1)
    moveInDir (withDir -> pos') what field' =
      case uncurry (getMatrix field') pos' of
        Nothing -> error "Unknown coordinate"
        Just e  -> case e of
          Empty -> Just (uncurry (setMatrix field' what) pos', pos')
          BoxL  -> if d == N || d == S then moveBoth False else moveOne
          BoxR  -> if d == N || d == S then moveBoth True  else moveOne
          Box   -> moveOne
          _     -> Nothing
          where
            moveOne = do
              (field'', _) <- moveInDir pos' e field'
              Just (uncurry (setMatrix field'' what) pos', pos')
            moveBoth b = do
              let posL = if b     then second pred pos' else pos'
                  posR = if not b then second succ pos' else pos'
              (field2 , _) <- moveInDir posL BoxL field'
              let field3 = uncurry (setMatrix field2 (if b then Empty else what)) posL
              (field4, _) <- moveInDir posR BoxR field3
              let field5 = uncurry (setMatrix field4 (if not b then Empty else what)) posR
              Just (field5, pos')

eval :: Matrix Field -> Int
eval = sum . V.imap (\y -> sum . V.imap (gps y))
  where
    gps y x Box  = x + 100 * y
    gps y x BoxL = x + 100 * y
    gps _ _ _   = 0

expand :: Matrix Field -> Matrix Field
expand = V.map (V.concatMap double)
  where
    double Border = V.fromList [Border, Border]
    double Empty  = V.fromList [Empty, Empty]
    double Box    = V.fromList [BoxL, BoxR]
    double Robot  = V.fromList [Robot, Empty]
    double _      = error "Unexpexted BoxR"

day15 :: IO ()
day15 = do
  input <- readFile "input/day15"
  case parse bathroom "" input of
    Left err       -> putStrLn $ displayException err
    Right (B {..}) -> do
      [pos] <- return $ findMatrix (==Robot) bMap
      putStr "Part 1: "
      print (eval (simulate bMap pos robotMoves))
      let bMap' = expand bMap
      [pos'] <- return $ findMatrix (==Robot) bMap'
      putStr "Part 2: "
      print (eval (simulate bMap' pos' robotMoves))

bathroom :: Parsec String () Bathroom
bathroom = B <$> mapP <*> (newline *> movesP)
  where
    mapP = V.fromList <$> line `endBy1` newline
    line = V.fromList <$> many1 field
    field = choice [ char '@' $> Robot, char '#' $> Border
                   , char '.' $> Empty, char 'O' $> Box ]
    movesP = concat <$> many1 dirP `endBy1` newline
    dirP = choice [ char '<' $> W, char '>' $> E
                  , char '^' $> N, char 'v' $> S ]
