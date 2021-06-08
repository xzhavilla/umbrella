module Main where

import Chromosome.FLC (FLC (FLC))
import Context (Context (Context), chromosomeSize, chromosomeType, pMutation, pSelection, populationSize)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Semigroup (stimes)
import Fitness (Fitness (Fitness))
import GHC.Float (int2Float)
import Gene.Bit (Bit (Zero))
import Lib (solve)
import System.Environment (getArgs)
import System.Random (newStdGen)

main :: IO ()
main = do
  args <- getArgs
  let s1 = head args
      s2 = args !! 1
      ln = max (length s1) (length s2)
  g <- newStdGen
  solve (strdiff s1 s2)
    `evalStateT` g
    `runReaderT` Context
      { chromosomeType = FLC,
        chromosomeSize = ln,
        populationSize = ln `div` 2,
        pSelection = 1 / 2,
        pMutation = 5 / 7
      }
  return ()

-- >>> strdiff "hello" "world" (FLC [One, One, One, Zero, One])
-- Infinity
strdiff s1 s2 (FLC xs) =
  let mx = max (length s1) (length s2)
      m = abs (length s1 - length s2)
      zipped = zip3 xs (s1 <> stimes m " ") (s2 <> stimes m " ")
      ff (bit, a, b) c
        | bit == Zero && a == b = c + 0
        | bit /= Zero && a /= b = c + 0
        | otherwise = c + 1
   in Fitness (int2Float mx / int2Float (foldr ff 0 zipped) -1)
