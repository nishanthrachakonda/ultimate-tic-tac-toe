{-# LANGUAGE DeriveFunctor #-}
module Utils where

------------------------------
----- / Position
------------------------------
type Position = Int

data Status = Success | Error

data Value = X | O
     deriving (Eq, Show)