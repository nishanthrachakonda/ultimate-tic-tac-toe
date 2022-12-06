{-# LANGUAGE DeriveFunctor #-}
module Board where
import qualified Data.Map as Map
import Grid
import Utils

-------------------------------
---- / Board
-------------------------------
type Board = Map.Map Position (GridStatus, Grid)

-------------------------------
----- / Actions
-------------------------------

putb :: Board -> (Position, Position) -> Value -> Status
putb board posT xo | state == Just Win X   = Error
                   | state == Just Win O   = Error
                   | state == Just Draw    = Error
                   | otherwise             = putg grid (snd posT) xo
                  where
                    gridT = Map.lookup (fst posT) board
                    state = fst gridT
                    grid  = snd gridT

----------------------------------
---- / State
----------------------------------
getbS :: Board -> GridStatus
getbS board | Map.size board == 9 = Draw
            | isbWon board X = Win X
            | isbWon board O = Win O
            | otherwise = Ongoing

isbWon :: Board -> Value -> Bool
isbWon board value = any and (winbpos board value)

winbpos :: Board -> Value -> [[Bool]]
winbpos board value = map (map (\x -> Map.lookup x board == Just value) ) (rwinpos ++ cwinpos ++ dwinpos ++ adwinpos)

rbwinpos :: [[Position]]
rbwinpos = [[3*i+j+1 | i <- [0, 1, 2]] | j <- [0, 1, 2]]

cbwinpos :: [[Position]]
cbwinpos = [[3*i+j+1 | j <- [0, 1, 2]] | i <- [0, 1, 2]]

dbwinpos :: [[Position]]
dbwinpos = [[4*i+1| i <- [0, 1, 2]]]

adbwinpos :: [[Position]]
adbwinpos = [[2*i+3| i <- [0, 1, 2]]]

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
            then pos+1
            else pos