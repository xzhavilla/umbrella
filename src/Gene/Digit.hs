module Gene.Digit where

import Control.Monad.State (runState, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import System.Random (Random, random, randomR)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

instance Bounded Digit where
  minBound = Zero
  maxBound = Nine

instance Random Digit where
  randomR (a, b) =
    randomR (toNum a :: Int, toNum b)
      & state
      <&> fromNum
      & runState
  random g = randomR (minBound, maxBound) g

fromNum n
  | n < 1 = Zero
  | n < 2 = One
  | n < 3 = Two
  | n < 4 = Three
  | n < 5 = Four
  | n < 6 = Five
  | n < 7 = Six
  | n < 8 = Seven
  | n < 9 = Eight
  | otherwise = Nine

toNum Zero = 0
toNum One = 1
toNum Two = 2
toNum Three = 3
toNum Four = 4
toNum Five = 5
toNum Six = 6
toNum Seven = 7
toNum Eight = 8
toNum Nine = 9
