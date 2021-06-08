module Fitness where

newtype Fitness = Fitness Float
  deriving (Eq, Ord)

instance Show Fitness where
  show (Fitness x) = show x

instance Bounded Fitness where
  minBound = Fitness (-1 / 0)
  maxBound = Fitness (1 / 0)
