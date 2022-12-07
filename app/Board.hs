{-# LANGUAGE DeriveFunctor #-}
module Board where
import qualified Data.Map as Map
import Grid
import Utils
import Data.Maybe 

-------------------------------
---- / Board
-------------------------------
type Board = Map.Map Int (GridStatus, Grid)

init :: Board
init = Map.empty

-------------------------------
----- / Actions
-------------------------------

putb :: Board -> Int -> CurPos -> Value -> (Status, Maybe (GridStatus, Board)) 
putb board ppos pos value | not (validgrid ppos gridT gridC (fst pos)) = (Error, Nothing) 
                          | fst gridI == Error = (Error, Nothing)
                          | otherwise = (Success, Just(getbS boardI, boardI))
                          where
                              gridT = Map.lookup (fst pos) board
                              gridC = Map.lookup ppos board
                              grid  | isNothing gridT = Map.empty
                                    | otherwise       = snd (fromJust gridT)
                              gridI = putg grid (snd pos) value
                              boardI = Map.insert (fst pos) (fromJust (snd gridI)) board  

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
winbpos board value = map (map (\p -> grideq value (Map.lookup p board))) (rwinpos ++ cwinpos ++ dwinpos ++ adwinpos)

grideq :: Value -> Maybe (GridStatus, Grid) -> Bool
grideq _ Nothing                   = False
grideq value (Just (gridStatus, _))  = Win value == gridStatus

validgrid :: Int -> Maybe (GridStatus, Grid) -> Maybe (GridStatus, Grid) -> Int ->  Bool
validgrid 0 _ _ _                    = True
validgrid p Nothing _ c              = p == c
validgrid p _ Nothing c              = p == c
validgrid p _ (Just (gridS, _)) c    = (p == c) && (gridS == Ongoing)
validgrid p (Just (gridS1, _)) (Just (gridS2, _)) c  = (gridS1 == Ongoing) && (gridS2 /= Ongoing)


rbwinpos :: [[Int]]
rbwinpos = [[3*i+j+1 | i <- [0, 1, 2]] | j <- [0, 1, 2]]

cbwinpos :: [[Int]]
cbwinpos = [[3*i+j+1 | j <- [0, 1, 2]] | i <- [0, 1, 2]]

dbwinpos :: [[Int]]
dbwinpos = [[4*i+1| i <- [0, 1, 2]]]

adbwinpos :: [[Int]]
adbwinpos = [[2*i+3| i <- [0, 1, 2]]]

----------------------------------
---- / Moves
----------------------------------

moveb :: (Int -> Int) -> CurPos -> CurPos
moveb dir pos | dir (snd pos) == snd pos = (dir (fst pos), 1)
              | otherwise                = (fst pos, dir (snd pos))

flipXO :: Value -> Value
flipXO X = O
flipXO O = X