{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Utils
import qualified Data.Map as Map

-------------------------------
---- / Grid
-------------------------------
type Grid = Map.Map Position Value

-------------------------------
----- / Actions
-------------------------------

put :: Grid -> Position -> Value -> Status
put grid pos xo = case Map.lookup pos grid of
                    Just _ -> Error
                    Nothing -> getS (Map.insert pos xo grid)


----------------------------------
---- / State
----------------------------------
getS :: Grid -> Status
getS _ = Success Draw
-- getS grid = do 
--                 if Map.size grid == 9
--                     then return Draw
--                     else if isWon grid X
--                     then return Win X
--                     else if isWon grid O
--                     then return Win O
--                     else return Ongoing

-- isWon :: Grid -> Value -> GridStatus
-- isWon grid value = Win X

----------------------------------
---- / Moves
----------------------------------

up :: Position -> Position
up pos = if pos-3 >= 1
            then pos-3
            else pos

down :: Position -> Position
down pos = if pos+3 <= 9
            then pos+3
            else pos

left :: Position -> Position
left pos = if pos-1 >= 1
            then pos-1
            else pos

right :: Position -> Position
right pos = if pos+1 <= 9
            then pos+9
            else pos