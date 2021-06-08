module Context where

data Context a b = Context
  { chromosomeType :: [a] -> b,
    chromosomeSize :: Int,
    populationSize :: Int,
    pSelection :: Float,
    pMutation :: Float
  }
