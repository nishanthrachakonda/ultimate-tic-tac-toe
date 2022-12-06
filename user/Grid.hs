{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Utils
import qualified Data.Map as Map
import Data.Maybe

-------------------------------
---- / Grid
-------------------------------
type Grid = Map.Map Int Value

-------------------------------
----- / Actions
-------------------------------

putg :: Grid -> Int -> Value -> (Status, Maybe (GridStatus, Grid))
putg grid pos xo | isJust (Map.lookup pos grid) = (Error, Nothing)
                 | otherwise = (Success, Just(getGS gridI, gridI))
                               where
                                gridI = Map.insert pos xo grid

----------------------------------
---- / State
----------------------------------
getGS :: Grid -> GridStatus
getGS grid | isWon grid X = Win X
           | isWon grid O = Win O
           | Map.size grid == 9 = Draw
           | otherwise = Ongoing

isWon :: Grid -> Value -> Bool
isWon grid value = any and (winpos grid value)

winpos :: Grid -> Value -> [[Bool]]
winpos grid value = map (map (\x -> Map.lookup x grid == Just value) ) (rwinpos ++ cwinpos ++ dwinpos ++ adwinpos)

rwinpos :: [[Int]]
rwinpos = [[3*i+j+1 | i <- [0, 1, 2]] | j <- [0, 1, 2]]

cwinpos :: [[Int]]
cwinpos = [[3*i+j+1 | j <- [0, 1, 2]] | i <- [0, 1, 2]]

dwinpos :: [[Int]]
dwinpos = [[4*i+1| i <- [0, 1, 2]]]

adwinpos :: [[Int]]
adwinpos = [[2*i+3| i <- [0, 1, 2]]]

----------------------------------
---- / Moves
----------------------------------

up :: Int -> Int
up pos = if pos-3 >= 1
            then pos-3
            else pos

down :: Int -> Int
down pos = if pos+3 <= 9
            then pos+3
            else pos

left :: Int -> Int
left pos = if pos-1 >= 1
            then pos-1
            else pos

right :: Int -> Int
right pos = if pos+1 <= 9
            then pos+1
            else pos
