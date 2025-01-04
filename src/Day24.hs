module Day24 where

import Control.Applicative ((<**>), (<|>))
import Control.Exception (displayException)
import Control.Monad (replicateM)
import Data.List (sort, isPrefixOf, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (Parsec, parse)
import Text.Parsec.Char (space, newline, char, string, alphaNum)
import Text.Parsec.Combinator (endBy1, choice)
import Text.Printf (printf)

import Utils (scc)

data Op = AND | OR | XOR
  deriving (Show, Eq, Ord)

data Exp = Exp Op String String String
  deriving (Eq)

isWrong :: [Exp] -> Exp -> Bool
isWrong es (Exp op lhs rhs res) =
  xorWithoutInOut || nonXorIsCarryRes || orAfterXOR || xorAfterAnd
  where
    children = filter (\c -> res == getLhs c || res == getRhs c) es
    childOps = map getOp children
    xorWithoutInOut = op == XOR && not (any isInOut [lhs, rhs, res])
    nonXorIsCarryRes = op /= XOR && isZ res && res /= "z45" -- except last
    orAfterXOR = OR `elem` childOps && op == XOR
    xorAfterAnd = XOR `elem` childOps && op == AND
                    && not (lhs == "x00" || rhs == "x00") -- except first

simulate :: [Exp] -> Map String Bool -> Map String Bool
simulate []                        m = m
simulate (Exp op s1 s2 res : exps) m = simulate exps (Map.insert res val m)
  where
    val = case op of
      AND -> m Map.! s1 && m Map.! s2
      OR  -> m Map.! s1 || m Map.! s2
      XOR -> m Map.! s1 /= m Map.! s2

mkNum :: Map String Bool -> Integer
mkNum = sum . map go . Map.toList
  where
    go (['z', d1, d2], True) = 2 ^ (read [d1, d2] :: Integer)
    go _ = 0

day24 :: IO ()
day24 = do
  input <- readFile "input/day24"
  case parse puzzle "" input of
    Left err -> putStrLn (displayException err)
    Right (initial, exps) -> do
      let components = concat $ scc (return . getRes) getArgs exps
      -- putStrLn (expsToDot components)
      putStr "Part 1: "
      print (mkNum (simulate components initial))
      putStr "Part 2: "
      putStrLn (intercalate "," $ sort $ map getRes $ filter (isWrong exps) exps)
      return ()

puzzle :: Parsec String () (Map String Bool, [Exp])
puzzle = mkP <$> (initial `endBy1` newline) <* newline
             <*> (operation `endBy1` newline)
  where
    initial = (,) <$> wire <* string ": " <*> bool
    wire = replicateM 3 alphaNum
    bool = (True <$ char '1') <|> (False <$ char '0')
    operation = wire <* space <**> op <* space <*> wire <* string " -> " <*> wire
    op = choice [Exp AND <$ string "AND", Exp OR <$ string "OR", Exp XOR <$ string "XOR"]
    mkP x y = (Map.fromList x, y)

getOp :: Exp -> Op
getOp (Exp op _ _ _) = op

getArgs :: Exp -> [String]
getArgs e = [getLhs e, getRhs e]

getLhs :: Exp -> String
getLhs (Exp _ lhs _ _) = lhs

getRhs :: Exp -> String
getRhs (Exp _ _ rhs _) = rhs

getRes :: Exp -> String
getRes (Exp _ _ _ res) = res

isInOut :: String -> Bool
isInOut s = isX s || isY s || isZ s

isX, isY, isZ :: String -> Bool
isX = ("x" `isPrefixOf`)
isY = ("y" `isPrefixOf`)
isZ = ("z" `isPrefixOf`)

expToDot :: Exp -> String
expToDot (Exp op a b out) =
  printf "%s -> %s [label=\"%s\"];\n%s -> %s [label=\"%s\"];\n" a out opS b out opS
  where opS = show op

expsToDot :: [Exp] -> String
expsToDot exps = "digraph G {\n" ++ concatMap expToDot (sort (map canonicalize exps)) ++ "}"

canonicalize :: Exp -> Exp
canonicalize (Exp o op1 op2 res) = Exp o (min op1 op2) (max op1 op2) res

instance Ord Exp where
  compare :: Exp -> Exp -> Ordering
  compare exp1 exp2 = compare (sort (getArgs exp1), getRes exp1, getOp exp1)
                              (sort (getArgs exp2), getRes exp2, getOp exp2)
