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

putg :: Grid -> Position -> Value -> Status
putg grid pos xo | isJust (Map.lookup pos grid) = Error
                 | otherwise = Success (getGS (Map.insert pos xo grid))

----------------------------------
---- / State
----------------------------------
getGS :: Grid -> GridStatus
getGS grid | Map.size grid == 9 = Draw
          | isWon grid X = Win X
          | isWon grid O = Win O
          | otherwise = Ongoing

isWon :: Grid -> Value -> Bool
isWon grid value = any and (winpos grid value)

winpos :: Grid -> Value -> [[Bool]]
winpos grid value = map (map (\x -> Map.lookup x grid == Just value) ) (rwinpos ++ cwinpos ++ dwinpos ++ adwinpos)

rwinpos :: [[Position]]
rwinpos = [[3*i+j+1 | i <- [0, 1, 2]] | j <- [0, 1, 2]]

cwinpos :: [[Position]]
cwinpos = [[3*i+j+1 | j <- [0, 1, 2]] | i <- [0, 1, 2]]

dwinpos :: [[Position]]
dwinpos = [[4*i+1| i <- [0, 1, 2]]]

adwinpos :: [[Position]]
adwinpos = [[2*i+3| i <- [0, 1, 2]]]

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
