{-# LANGUAGE RecordWildCards #-}
module Day16 where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Trans.State (runState, modify)
import Data.PQueue.Min (insert, singleton, MinQueue((:<), Empty))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Control.Arrow (first)

import Day6 (Dir (..))
import Day10 (trd3)
import Utils (findMatrix, getMatrix, Matrix)

data State a = State { dir :: Dir, x :: Int, y :: Int, previous :: Pos, ann :: a }
  deriving (Eq, Show)


instance Ord a => Ord (State a) where
  compare = compare `on` ann

type Pos = (Int, Int, Dir)

shortestPaths :: Matrix Char -> ((Int, Map Pos Int), Map Pos (Set Pos))
shortestPaths m = runState (dijkstra (singleton initial) Map.empty) Map.empty
  where
    initial = case findMatrix (=='S') m of
      []  -> error "No starting tile"
      (x, y):_ -> State N x y (-1, -1, N) 0
    dijkstra Empty              vis = return (maxBound, vis)
    dijkstra (State {..} :< xs) vis = case Map.lookup (x, y, dir) vis of
      Just n | n <= ann -> do
        when (n == ann) (modify (Map.insertWith Set.union (x, y, dir)
                                  (Set.singleton previous)))
        dijkstra xs vis
      _ -> do
        modify (Map.insertWith Set.union (x, y, dir) (Set.singleton previous))
        let neighbors = [State dir x' y' (x, y, dir) (ann + 1) | let (x', y') = goDir dir x y
                                                   , Just c <- return $ getMatrix m x' y'
                                                   , c /= '#'] ++
                        [State dir' x y (x, y, dir) (ann + 1000) | dir' <- turn dir]
        let queue' = foldr insert xs neighbors
        let vis' = Map.insert (x, y, dir) ann vis
        case getMatrix m x y of
          Just 'E' -> first (min ann) <$> dijkstra queue' vis'
          _        -> dijkstra queue' vis'

    goDir N x y = (x, y - 1)
    goDir S x y = (x, y + 1)
    goDir E x y = (x + 1, y)
    goDir W x y = (x - 1, y)

    turn d = [toEnum (pred (fromEnum d) `mod` 4), toEnum (succ (fromEnum d) `mod` 4)]

followPaths :: Map Pos (Set Pos) -> Pos -> (Int, Int) -> Set (Int, Int)
followPaths parents current' goal = go current'
  where
    go current@(x', y',_)
      | (x', y') == goal = Set.singleton (x', y')
      | otherwise        = Set.insert (x', y') $
                           Set.unions [go p | Just ps <- return $ Map.lookup current parents
                                            , p <- Set.toList ps]

day16 :: IO ()
day16 = do
  input <- V.fromList . Prelude.map V.fromList . lines <$> readFile "input/day16"
  case (findMatrix (=='E') input, findMatrix (=='S') input) of
    ((x', y'):_, s:_) -> do
      let ((len, visited), paths) = shortestPaths input
      putStr "Part 1: "
      print len
      let minDirs = map trd3 $ Map.keys $
            Map.filterWithKey (\(x'', y'', _) n -> (x', y', len) == (x'', y'', n)) visited
      putStr "Part 2: "
      print (Set.size (Set.unions [followPaths paths (x', y', d) s | d <- minDirs]))
    _   -> error "no start or end tile"
