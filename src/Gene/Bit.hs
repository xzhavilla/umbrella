module Gene.Bit where

import Binary (Binary, other)
import Control.Monad.State (runState, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import Mutation (Mutation, mutate)
import System.Random (Random, random, randomR)

data Bit = Zero | One
  deriving (Eq, Ord)

instance Binary Bit where
  other Zero = One
  other One = Zero

instance Bounded Bit where
  minBound = Zero
  maxBound = One

instance Mutation Bit where
  mutate p x = do
    n <- state random
    if n < p
      then return $ other x
      else return x

instance Random Bit where
  randomR (a, b) =
    randomR (toBool a, toBool b)
      & state
      <&> fromBool
      & runState
  random g = randomR (minBound, maxBound) g

instance Show Bit where
  show Zero = "."
  show One = "#"

fromBool False = Zero
fromBool True = One

toBool Zero = False
toBool One = True
