{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Utils
import qualified Data.Map as Map
import Data.Maybe

-------------------------------
---- / Grid
-------------------------------
type Grid = Map.Map Position Value

-------------------------------
----- / Actions
-------------------------------

put :: Grid -> Position -> Value -> Status
put grid pos xo | isJust (Map.lookup pos grid) = Error
                | otherwise = Success (getS (Map.insert pos xo grid))


----------------------------------
---- / State
----------------------------------
getS :: Grid -> GridStatus
getS grid | Map.size grid == 9 = Draw
          | isWon grid X = Win X
          | isWon grid O = Win O
          | otherwise = Ongoing

isWon :: Grid -> Value -> Bool
isWon grid value = any and (rwinpos grid value)

-- iswinning :: grid -> [Position] -> Bool
-- iswinning grid posL = any and (rwinpos grid X)

rwinpos :: Grid -> Value -> [[Bool]]
rwinpos grid value = [[grid Map.! 3*i+j+1 == value  | i <- [0, 1, 2]] | j <- [0, 1, 2]]

cwinpos :: Grid -> Value -> [[Bool]]
cwinpos grid value = [[grid Map.! 3*i+j+1 == value | j <- [0, 1, 2]] | i <- [0, 1, 2]]

dwinpos :: Grid -> Value -> [[Bool]]
dwinpos grid value = [[grid Map.! 4*i+1 == value | i <- [0, 1, 2]]]

adwinpos :: Grid -> Value -> [[Bool]]
adwinpos grid value = [[grid Map.! 2*i+3 == value | i <- [0, 1, 2]]]

-- >>> 

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
