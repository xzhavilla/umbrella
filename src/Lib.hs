{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Generation (Generation (Generation), initial, next)
import Population (hasSolution)

solve f = initial f >>= eval f

eval f gen@(Generation _ xs) = do
  liftIO $ print gen
  if hasSolution xs
    then return gen
    else next f gen >>= eval f
