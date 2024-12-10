{-# LANGUAGE LambdaCase #-}
module Day9 where

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
compactFile xs' = expand $ snd $
  iterateUntil (moveOne blocks) (Map.size blocks - 1, gathered)
  where
    (blocks, gathered) = gather 0 xs'
    iterateUntil f e = case f e of
      Nothing -> e
      Just e' -> iterateUntil f e'
    moveOne _ (0, _) = Nothing
    moveOne m (n, s) = case Map.lookup n m of
      Nothing   -> error $ "invalid block ID: " ++ show n
      Just size -> Just (pred n, fitIn s size n)
    fitIn [] _ _ = []
    fitIn s@(v@(Right (n',_)) : rest) size n
      | n' == n   = s
      | otherwise = v : fitIn rest size n
    fitIn (v@(Left size')      : rest) size n
      | size' == size = Right (n, size) : foldr replace [] rest
      | size' >= size = Right (n, size) : Left (size'-size) : foldr replace [] rest
      | otherwise = v : fitIn rest size n
      where
        replace (Right (w, s')) xs | w == n = Left s' <:> xs
        replace e xs = e <:> xs
        Left s1 <:> (Left s2 : xs) = Left (s1 + s2) : xs
        x <:> xs = x : xs
    gather _     []       = (Map.empty, [])
    gather block [x]      = (Map.singleton block x, [Right (block, x)])
    gather block (x:y:ys) = (Map.insert block x m',
      Right (block,x) : Left y : s')
      where
        (m', s') = gather (succ block) ys
    expand = concatMap $ \case
      Left n       -> replicate n 0
      Right (v, n) -> replicate n v

checksum :: [Int] -> Int
checksum xs = sum (zipWith (*) xs [0 ..])

-- >>> day9
day9 :: IO ()
day9 = do
  input <- map (read . return) . init <$> readFile "input/day9"
  print (checksum (compactBlock input))
  print (checksum (compactFile input))
