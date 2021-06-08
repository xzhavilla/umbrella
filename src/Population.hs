{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Population where

import Context (pSelection)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, sort)
import Fitness (Fitness (Fitness))
import GHC.Float (int2Float)
import Individual (Individual (Individual), isSolution)
import List (unzipAdjacent, zipAdjacent)
import Mix (mix)
import Mutation (Mutation, mutate)

newtype Population a = Population [Individual a]
  deriving (Foldable, Functor, Semigroup, Traversable)

instance Mutation a => Mutation (Population a) where
  mutate p (Population xs) = Population <$> traverse (mutate p) xs

instance (Ord a, Show a) => Show (Population a) where
  show (Population xs) =
    sort xs
      & reverse
      <&> show
      & intercalate "\n"

-- >>> fit Fitness (Population [Individual minBound 0, Individual minBound 1])
-- 1.0	(1.0)
-- 0.0	(0.0)
fit f (Population xs) = Population $ fmap (\(Individual _ x) -> Individual (f x) x) xs

select (Population xs) = do
  n <- asks pSelection
  return $
    sort xs
      & reverse
      & (\xs -> take (floor $ (length xs & int2Float) * n) xs)
      & Population

crossOver (Population xs) =
  zipAdjacent xs
    & traverse mix
    <&> unzipAdjacent
    <&> Population

best (Population xs) = maximum xs

hasSolution (Population xs) = any isSolution xs
