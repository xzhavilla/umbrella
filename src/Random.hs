{-# LANGUAGE TupleSections #-}

module Random where

import Data.Function ((&))
import Data.List.Split (chunksOf)
import System.Random (randoms, split)

-- >>> randomNM 2 3 (mkStdGen 0) :: ([[Float]], StdGen)
-- ([[0.74242944,0.1323092,1.3599575e-2],[0.42590684,0.28189754,0.71788645]],2 40692)
randomNM n m g =
  randoms g
    & chunksOf m
    & take n
    & (,g')
  where
    (g', _) = split g
