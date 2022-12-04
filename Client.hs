-- State = List of 9 grids
-- Grid = List of 9 Ints

-- Msg = Move | ConnectAck | DisconnectAck

-- Encoding for cell status:
-- 0 is X
-- 1 is O
-- 2 is empty 
-- 3 is blocked square (inelegible by rules)


-- CLIENT NETWORKING
-- sendJoinRequest
-- sendMove - sends (x,y,c)
-- x,y is coordinate, c is X or O
-- listenMsg


-- GAME ENGINE
-- getValidMoves (State) -> [(x,y)]
-- hasWon :: (State, Grid) -> Bool
-- processRcvdMove (x,y,c)


-- brick library
-- Render the entire updated state 
-- render :: (State) -> Display
-- Also input 

