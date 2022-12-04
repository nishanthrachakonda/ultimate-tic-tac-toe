{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Utils
import qualified Data.Map as Map

-------------------------------
---- / Grid
-------------------------------
type Grid = Map Position Value

-------------------------------
----- / Actions
-------------------------------

put :: Grid -> Position -> Value -> Status
put grid pos xo = do
                value <- Map.lookup grid pos
                if isJust value
                    then return Error
                    else do 
                        Map.insert pos xo grid
                        return Success

