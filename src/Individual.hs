{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Individual where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Fitness (Fitness)
import Mix (Mix, mix)
import Mutation (Mutation, mutate)

data Individual a = Individual Fitness a
  deriving (Eq, Foldable, Functor, Traversable)

instance Mix a => Mix (Individual a) where
  mix (Individual _ a, Individual _ b) =
    mix (a, b)
      <&> bimap (Individual minBound) (Individual minBound)

instance Mutation a => Mutation (Individual a) where
  mutate p (Individual _ x) =
    mutate p x
      <&> Individual minBound

instance Ord a => Ord (Individual a) where
  compare (Individual n x) (Individual n' x')
    | n == n' = compare x x'
    | otherwise = compare n n'

instance Show a => Show (Individual a) where
  show (Individual n x) = show x ++ "\t(" ++ show n ++ ")"

isSolution (Individual n _)
  | n == maxBound = True
  | otherwise = False
