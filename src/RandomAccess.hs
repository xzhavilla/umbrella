module RandomAccess where

import Data.Function ((&))
import System.Random (RandomGen, randomR)

class Foldable t => RandomAccess t where
  randomIndex :: RandomGen g => t a -> g -> (Int, g)
  randomIndex xs g =
    length xs
      & \n -> randomR (0, n - 1) g
