{-# LANGUAGE DeriveFunctor #-}
module Board where
import qualified Data.Map as Map

-------------------------------
---- / Board
-------------------------------
type Board = Map Position (GridStatus, Grid)

data GridStatus = Win Value | Draw | Ongoing
     deriving (Eq, Show)

-------------------------------
---- / Action
-------------------------------
put :: Board -> (Position, Position) -> Status
put board posT = put grid bpos
                where 
                    grid = get board pos

get :: Board -> Position -> Grid
get board pos = board ! bpos
                 where
                    bpos = getbpos pos
