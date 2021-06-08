module Blend where

import Control.Monad.State (StateT)
import System.Random (RandomGen)

class Blend a where
  blend :: (RandomGen g, Monad m) => (a, a) -> StateT g m (a, a)
