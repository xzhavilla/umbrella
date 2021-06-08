{-# LANGUAGE FlexibleContexts #-}

module Generation where

import Chromosome.FLC (FLC (FLC))
import Context (Context (Context), chromosomeSize, chromosomeType, pMutation, populationSize)
import Control.Monad.Reader (asks, runReader)
import Control.Monad.State (runStateT, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import Fitness (Fitness)
import Gene.Bit (Bit)
import Individual (Individual (Individual))
import Mutation (Mutation, mutate)
import Population (Population (Population), best, crossOver, fit, hasSolution, select)
import Random (randomNM)
import System.Random (StdGen, mkStdGen)

data Generation a = Generation Int (Population a)

newtype VerboseGeneration a = VerboseGeneration (Generation a)

instance (Ord a, Show a) => Show (Generation a) where
  show gen@(Generation n xs) = prefix gen ++ show n ++ "#\t" ++ show (best xs)

instance (Ord a, Show a) => Show (VerboseGeneration a) where
  show (VerboseGeneration gen@(Generation n xs)) = show xs ++ "\n" ++ show gen

-- >>> initial (\_ -> minBound) `runStateT` mkStdGen 0 `runReader` Context FLC 3 10 :: (Generation (FLC Bit), StdGen)
-- (>¦  0#	[ # # # ]	(-Infinity),2 40692)
initial f = do
  c <- asks chromosomeType
  m <- asks chromosomeSize
  n <- asks populationSize
  randomNM n m
    & state
    <&> fmap c
    <&> fmap (\x -> Individual (f x) x)
    <&> Population
    <&> Generation 0

-- >>> (initial (\_ -> minBound) >>= next (\_ -> maxBound)) `runStateT` mkStdGen 0 `runReader` Context FLC 3 10 :: (Generation (FLC Bit), StdGen)
-- ( ¦> 1#	[ # # # ]	(Infinity),644449386 1207612141)
next f (Generation n xs) = do
  parents <- select xs
  children <- crossOver parents
  p <- asks pMutation
  children <- mutate p children
  let children' = fit f children
  return (Generation (n + 1) (parents <> children'))

isInitial (Generation n _) = n == 0

prefix gen@(Generation _ xs)
  | hasSolution xs = " ¦> "
  | isInitial gen = ">¦  "
  | otherwise = " ¦  "
