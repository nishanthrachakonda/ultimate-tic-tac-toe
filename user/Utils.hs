{-# LANGUAGE DeriveFunctor #-}
module Utils where

------------------------------
----- / Position
------------------------------
type CurPos = (Int, Int)

data GridStatus = Win Value | Draw | Ongoing
     deriving (Eq, Show)

data Status = Success GridStatus | Error
     deriving (Eq, Show)

data Value = X | O
     deriving (Eq, Show)