module Day23 where

import Data.Algorithm.MaximalCliques (maximalCliques)
import Data.List (intercalate, sort, sortOn)
import Data.List.Extra (nubOrd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

toIntSet :: [(String, String)] -> (IntSet, Map String Int, Map Int String, Map Int IntSet)
toIntSet [] = (Set.empty, Map.empty, Map.empty, Map.empty)
toIntSet ((x, y):xs) =
  let (iS, rm, m, n) = toIntSet xs
      idX = Map.findWithDefault (Set.size iS) x rm
      newIS = Set.insert idX iS
      idY = Map.findWithDefault (Set.size newIS) y rm
      newIS' = Set.insert idY newIS
      newM = Map.insert idX x . Map.insert idY y $ m
      newRM = Map.insert x idX . Map.insert y idY $ rm
      newN = Map.insertWith Set.union idX (Set.singleton idY) n
      newN' = Map.insertWith Set.union idY (Set.singleton idX) newN
  in (newIS', newRM, newM, newN')

getCliques3 :: IntSet -> Map Int String -> Map Int IntSet -> [IntSet]
getCliques3 s mapping neigh = filter connected combinations3
  where
    connected s' = all (\e -> 1 == Set.size (s' Set.\\ (neigh Map.! e))) (Set.toList s')
    combinations3 = nubOrd
      [ Set.fromList [x, y, z] | x <- Set.toList (Set.filter histComp s)
                               , y <- Set.toList (neigh Map.! x)
                               , z <- Set.toList (Set.delete x (neigh Map.! y))]
    histComp i = case Map.lookup i mapping of
      Just ('t':_) -> True
      _            -> False

password :: Map Int String -> IntSet -> String
password mapping = intercalate "," . sort . map (mapping Map.! ) . Set.toList

day23 :: IO ()
day23 = do
  let read' s = let (a, b) = span (/='-') s in (a, drop 1 b)
  input <- map read' . lines <$> readFile "input/day23"
  let (iSet, _, mapping, neigh) = toIntSet input
  let biggestClique = last $ sortOn Set.size $
                      maximalCliques (\s1 s2 -> Set.findMax (Set.union s1 s2))
                                     (\i -> Map.findWithDefault Set.empty i neigh)
                                     iSet
  putStr "Part 1: "
  print (length (getCliques3 iSet mapping neigh))
  putStr "Part 2: "
  putStrLn (password mapping biggestClique)
