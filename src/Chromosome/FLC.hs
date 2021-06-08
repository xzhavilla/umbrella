{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Chromosome.FLC where

import Control.Monad.State (runState, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Index (setAt)
import Gene.Bit (Bit (One, Zero))
import Mix (Mix, mix)
import Mutation (Mutation, mutate)
import RandomAccess (RandomAccess, randomIndex)
import System.Random (mkStdGen, random, randomR)

newtype FLC a = FLC [a]
  deriving (Eq, Foldable)

instance Mix (FLC a) where
  mix (flc@(FLC as), FLC bs) = do
    i <- state $ randomIndex flc
    let cs = take i bs ++ drop i as
    let ds = take i as ++ drop i bs
    return (FLC cs, FLC ds)

-- >>> runState (mutate 1 (FLC [One,Zero,One])) (mkStdGen 0)
-- ([ Zero Zero One ],732249858 652912057)
instance Mutation a => Mutation (FLC a) where
  mutate p flc@(FLC xs) = do
    n <- state random
    if n < p
      then do
        i <- state $ randomIndex flc
        mutate (1 :: Float) (xs !! i)
          <&> (\x -> setAt i x xs)
          <&> FLC
      else return flc

instance Ord a => Ord (FLC a) where
  compare (FLC xs) (FLC xs') = compare xs xs'

instance RandomAccess FLC

instance Show a => Show (FLC a) where
  show (FLC xs) = "[ " ++ unwords (show <$> xs) ++ " ]"
