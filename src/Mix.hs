module Mix where

import Control.Monad.State (StateT)
import System.Random (RandomGen)

class Mix a where
  mix :: (RandomGen g, Monad m) => (a, a) -> StateT g m (a, a)
