module Day9 where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

compactBlock :: [Int] -> [Int]
compactBlock xs' = go1 0 xs' filler
  where
    filler = reverse (concat [replicate x (i `div` 2) | (i, x) <- zip [0..] xs', even i])
    go1 block (x : xs) fill@(i:_)
      | block == i = takeWhile (==i) fill
      | otherwise = replicate x block ++ go2 (succ block) xs fill
    go1 _ _ _ = error "not enough filler"
    go2 block (x:xs) is =
      let (s, e) = splitAt x is
      in s ++ go1 block xs e
    go2 _ [] _ = []

compactFile :: [Int] -> [Int]
compactFile xs' = foldr ((:) . fromMaybe 0) [] $ snd $
  iterateUntil (moveOne blocks) (Map.size blocks - 1, expanded)
  where
    (blocks, expanded) = expand 0 xs'
    iterateUntil f e = case f e of
      Nothing -> e
      Just e' -> iterateUntil f e'
    moveOne _ (0, _) = Nothing
    moveOne m (n, s) = case Map.lookup n m of
      Nothing   -> error $ "invalid block ID: " ++ show n
      Just size -> Just (pred n, fitIn s (replicate size Nothing) size n)
    fitIn [] _ _ _ = []
    fitIn s@(Just n':_) _ _    n
      | n' == n             = s
    fitIn s@(f:rest) what size n
      | what `isPrefixOf` s = replicate size (Just n) ++ replaced
      | otherwise           = f : fitIn rest what size n
      where
        replaced = map (\e -> if e == Just n then Nothing else e) (drop size s)
    expand _     []       = (Map.empty, [])
    expand block [x]      = (Map.singleton block x, replicate x (Just block))
    expand block (x:y:ys) = (Map.insert block x m',
      replicate x (Just block) ++ replicate y Nothing ++ s')
      where
        (m', s') = expand (succ block) ys

checksum :: [Int] -> Int
checksum xs = sum (zipWith (*) xs [0 ..])

day9 :: IO ()
day9 = do
  input <- map (read . return) . init <$> readFile "input/day9"
  print (checksum (compactBlock input))
  print (checksum (compactFile input))
