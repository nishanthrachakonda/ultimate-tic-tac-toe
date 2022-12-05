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
isWon grid value = True

iswinning :: grid -> [Position] -> Bool
iswinning grid posL = grid 

-- winpos :: [[Position]]
-- winpos = rwinpos ++ cwinpos ++ dwinpos ++ adwinpos

rwinpos :: Grid -> Value -> [[Bool]]
rwinpos grid value = [[grid Map.! 3*i+j+1 == value | i <- [0..2]] | j <- [0..2]]

cwinpos :: [[Position]]
cwinpos = [[3*i+j+1 | j <- [0..2]] | i <- [0..2]]

dwinpos :: [[Position]]
dwinpos = [[4*i+1 | i <- [0..2]]]

-- >>> cwinpos
-- [[1,4,7],[2,5,8],[3,6,9],[1,2,3],[4,5,6],[7,8,9],[1,5,9],[3,5,7]]

adwinpos :: [[Position]]
adwinpos = [[2*i+3 | i <- [0..2]]]

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
