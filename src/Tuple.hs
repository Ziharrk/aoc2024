{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}
module Tuple where

import Prelude hiding (uncurry, curry)
import Control.Monad (replicateM)
import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)

class Tuple a where
  type UncurriedFunction a c :: Type
  uncurry :: UncurriedFunction a c -> a -> c
  curry :: (a -> c) -> UncurriedFunction a c

do  let genTupleInstance :: Int -> Q Dec
        genTupleInstance arity = do
            vs <- replicateM arity (newName "t")
            let tvs = map VarT vs
            v <- newName "res"
            f <- newName "f"
            let mkFunTy = foldr1 (AppT . AppT ArrowT)
            let tupleTy = foldl AppT (TupleT arity) tvs
            let funTy = mkFunTy (tvs ++ [VarT v])
            let ty = TySynInstD $ TySynEqn Nothing
                        (foldl AppT (ConT ''UncurriedFunction) [tupleTy, VarT v])
                        funTy
            let unSig = SigD 'uncurry $ mkFunTy [funTy, tupleTy, VarT v]
            let un = FunD 'uncurry [Clause [VarP f, TupP (map VarP vs)]
                        (NormalB $ foldl AppE (VarE f) (map VarE vs))
                        []]
            let cuSig = SigD 'curry $ mkFunTy (mkFunTy [tupleTy, VarT v] : tvs ++ [VarT v])
            let cu = FunD 'curry [Clause (VarP f : map VarP vs)
                        (NormalB $ AppE (VarE f) (TupE (map (Just . VarE) vs)))
                        []]
            return $ InstanceD Nothing []
                        (AppT (ConT ''Tuple) tupleTy)
                        [ty, unSig, un, cu, cuSig]
    mapM genTupleInstance [(2 :: Int) .. 64]

{-
generates the following code:

instance Tuple (a, b) where
  type UncurriedFunction (a, b) c = a -> b -> c
  uncurry f (a, b) = f a b
  curry f a b = f (a, b)

and every other instance up to tuple arity 64
-}

-- | Uncurry implemented for arity 16 tuples,
-- using one of the instances generated via the TH code above
uncurry16 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q)
          -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
          -> q
uncurry16 = uncurry

-- | Curry implemented for arity 16 tuples,
-- using one of the instances generated via the TH code above
curry16 :: ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> q)
          -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p
          -> q
curry16 = curry
