module Day5 where

import Data.List.Extra (split, partition)
import Data.Map (Map)
import qualified Data.Map as Map

import Utils (scc)

checkUpdate :: Map Int [Int] -> [[Int]] -> ([[Int]], [[Int]])
checkUpdate rules = partition (check [])
  where
    check _  []     = True
    check ys (x:xs) = all (`notElem` ys) (Map.findWithDefault [] x rules) && check (x:ys) xs

reorder :: Map Int [Int] -> [[Int]] -> [[Int]]
reorder rules = map reorderOne
  where
    reorderOne xs =
      -- strongly connected components of the implied graph
      insertComponent (scc (\e -> Map.findWithDefault [] e rules) return xs) []
    insertComponent []       ys = ys
    insertComponent ([x]:xs) ys = insertComponent xs (x:ys)
    insertComponent _        _  = error "no greedy solution"

updateValue :: [[Int]] -> Int
updateValue = sum . map (\xs -> xs !! (length xs `div` 2))

day5 :: IO ()
day5 = do
  input <- lines <$> readFile "input/day5"
  let (ruleLines, updateLines) = span (/="") input
      rules = map (tup . split (=='|')) ruleLines
      updates = map (\e -> read ('[' : e ++ "]")) (tail updateLines)

      tup [x,y] = (read x, read y)
      tup _     = undefined

      ruleMap = foldr (\(a, b) -> Map.insertWith (++) a [b]) Map.empty rules
      (valid, invalid) = checkUpdate ruleMap updates
  putStr "Part 1: "
  print $ updateValue valid
  putStr "Part 2: "
  print $ updateValue (reorder ruleMap invalid)
