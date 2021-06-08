module Mutation where

import Control.Monad.State (StateT)
import System.Random (Random, RandomGen)

class Mutation a where
  mutate :: (Fractional p, Ord p, Random p, RandomGen g, Monad m) => p -> a -> StateT g m a
