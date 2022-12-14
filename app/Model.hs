{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import Utils

import Board
import Network.Socket
-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------
  
data PlayState = PS
  { psX      :: Player 
  , psO      :: Player 
  , psBoard  :: Board     
  , psTurn   :: Value     
  , psCur    :: CurPos    
  , psPos    :: Int
  , psPlayerNum :: Int
  , psIsMyTurn :: Int
  , psConn   :: Socket
  , psMessage :: String
  , psGameState :: GridStatus
  }

init :: PlayState
init = PS {
    psX = Local,
    psO = Server,
    psBoard = Board.init,
    psTurn = X, -- change this on ack from server
    psCur = (1, 1),
    psPos = 0,
    psPlayerNum = 0, -- change this on ack from server
    psIsMyTurn = 0,
    psMessage = "Sending connection request to server",
    psGameState = Ongoing
}

data Player = Local | Server
  deriving (Eq)

playerName :: PlayState -> String
playerName ps | psX ps == Local = "X"
              | otherwise = "O"

isCurr :: PlayState -> Utils.CurPos -> Bool
isCurr ps cp = (psCur ps) == cp