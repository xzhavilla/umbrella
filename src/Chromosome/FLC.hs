{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Chromosome.FLC where

import Blend (Blend, blend)
import Control.Monad.State (state)
import Data.Functor ((<&>))
import Data.List.Index (setAt)
import Mutation (Mutation, mutate)
import RandomAccess (RandomAccess, randomIndex)
import System.Random (random)

newtype FLC a = FLC [a]
  deriving (Eq, Foldable)

instance Blend (FLC a) where
  blend (chr@(FLC as), FLC bs) = do
    i <- state $ randomIndex chr
    let cs = take i bs ++ drop i as
    let ds = take i as ++ drop i bs
    pure (FLC cs, FLC ds)

-- >>> mutate (1 :: Float) (FLC [One, Zero, One]) `runStateT` mkStdGen 0
-- ([ . . # ],732249858 652912057)
instance Mutation a => Mutation (FLC a) where
  mutate p chr@(FLC xs) = do
    n <- state random
    if n < p
      then do
        i <- state $ randomIndex chr
        mutate (1 :: Float) (xs !! i)
          <&> (\x -> setAt i x xs)
          <&> FLC
      else return chr

instance Ord a => Ord (FLC a) where
  compare (FLC xs) (FLC xs') = compare xs xs'

instance RandomAccess FLC

instance Show a => Show (FLC a) where
  show (FLC xs) = "[ " ++ unwords (show <$> xs) ++ " ]"
