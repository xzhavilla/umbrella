{-# LANGUAGE FlexibleContexts #-}

module Debug where

newtype Debug a = Debug a

debug x = print $ Debug x
