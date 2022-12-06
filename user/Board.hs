{-# LANGUAGE DeriveFunctor #-}
module Board where
import qualified Data.Map as Map
import Grid
import Utils

-------------------------------
---- / Board
-------------------------------
type Board = Map.Map Int (GridStatus, Grid)

-------------------------------
----- / Actions
-------------------------------

putb :: Board -> CurPos -> Value -> Status
putb board pos value | state == Just Win X = Error
                     | state == Just Win O = Error
                     | state == Just Draw  = Error
                     | otherwise           = putg grid (snd pos) value
                  where
                    gridT = Map.lookup (fst pos) board
                    state = fst gridT
                    grid  = snd gridT

----------------------------------
---- / State
----------------------------------
getbS :: Board -> GridStatus
getbS board | isbWon board X      = Win X
            | isbWon board O      = Win O
            | Map.size board == 9 = Draw
            | otherwise           = Ongoing

isbWon :: Board -> Value -> Bool
isbWon board value = any and (winbpos board value)

winbpos :: Board -> Value -> [[Bool]]
winbpos board value = map (map (\x -> Map.lookup x board == Just (value, _)) ) (rwinpos ++ cwinpos ++ dwinpos ++ adwinpos)

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

moveb :: (Int -> Int) -> Position -> Position
moveb dir pos | dir snd pos == snd pos = (dir fst pos, 1)
              | otherwise              = (fst pos, dir snd pos)