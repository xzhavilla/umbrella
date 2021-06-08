{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Individual where

import Blend (Blend, blend)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Fitness (Fitness)
import Mutation (Mutation, mutate)

data Individual a = Individual Fitness a
  deriving (Eq, Foldable, Functor, Traversable)

instance Blend a => Blend (Individual a) where
  blend (Individual _ a, Individual _ b) =
    blend (a, b)
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
