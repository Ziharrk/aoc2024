module Day21 where

import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

buttonLayoutNum :: ([(Char, Position)], Position)
buttonLayoutNum =
  ( [ ('7', (0, 0)), ('8', (0, 1)), ('9', (0, 2))
    , ('4', (1, 0)), ('5', (1, 1)), ('6', (1, 2))
    , ('1', (2, 0)), ('2', (2, 1)), ('3', (2, 2))
                   , ('0', (3, 1)), ('A', (3, 2))
    ]
  , (3, 0) )

buttonLayoutDir :: ([(Char, Position)], Position)
buttonLayoutDir =
  ( [                ('^', (0, 1)), ('A', (0, 2))
    , ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
    ]
  , (0, 0) )

getSimplePath :: (Int, Int) -> (Char, (Int, Int)) -> (Char, (Int, Int))
              -> ((Char, Char), String)
getSimplePath (bx, by) (c1, (x1, y1)) (c2, (x2, y2))
  | (dy > 0 || bx == x1 && by == y2) &&
              (bx /= x2 || by /= y1)
              = ((c1, c2), v ++ h ++ "A")
  | otherwise = ((c1, c2), h ++ v ++ "A")
  where
    (dx, dy) = (x2 - x1, y2 - y1)
    v = replicate (-dx) '^' ++ replicate dx 'v'
    h = replicate (-dy) '<' ++ replicate dy '>'

simplePaths :: Map (Char, Char) String
simplePaths = foldr insert Map.empty [buttonLayoutDir, buttonLayoutNum]
  where
    insert (layout, empx2T) m =
      foldr (uncurry Map.insert) m [getSimplePath empx2T p1 p2 | p1 <- layout, p2 <- layout]

allPathsWithDepth :: Int -> Map (Char, Char, Int) Int
allPathsWithDepth depth = foldl update inital [1..depth-1]
  where
    initMapFor ((c1, _), (c2, _)) = Map.insert (c1, c2, 0) (length (simplePaths Map.! (c1, c2)))
    inital = foldr initMapFor Map.empty [(p1, p2) | p1 <- fst buttonLayoutDir
                                                  , p2 <- fst buttonLayoutDir]
    updateFor d ((c1, _), (c2, _)) m = Map.insert (c1, c2, d) dist m
      where
        reqAct = simplePaths Map.! (c1, c2)
        dist = sum $ zipWith (getAtDepth m (d - 1)) ('A' : reqAct) reqAct
    update m d = foldr (updateFor d) m [(p1, p2) | p1 <- fst buttonLayoutDir
                                                 , p2 <- fst buttonLayoutDir]

getCodeLength :: Map (Char, Char, Int) Int -> Int -> [Char] -> Int
getCodeLength depthMap depth code =
  sum $ zipWith (getAtDepth depthMap (depth - 1)) ('A' : reqAct) reqAct
  where
    reqAct = concat (zipWith (curry (simplePaths Map.!)) ('A' : code) code)

getAtDepth :: Map (Char, Char, Int) Int -> Int -> Char -> Char -> Int
getAtDepth m d f t = m Map.! (f, t, d)

getRes :: Map (Char, Char, Int) Int -> [[Char]] -> [Int] -> Int -> Int
getRes depthMap input input' n =
  sum $ zipWith (*) input' $ map (getCodeLength depthMap n ) input

day21 :: IO ()
day21 = do
  input <- lines <$> readFile "input/day21"
  let read'  = read . dropWhile (=='0') . take 3
      input' = map read' input
      depthMap = allPathsWithDepth 25
  putStr "Part 1: "
  print (getRes depthMap input input' 2)
  putStr "Part 2: "
  print (getRes depthMap input input' 25)
