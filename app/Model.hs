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
  , psConn   :: Socket
  }

init :: PlayState
init = PS {
    psX = Local,
    psO = Server,
    psBoard = Board.init,
    psTurn = X,
    psCur = (1, 1),
    psPos = 0
}

data Player = Local | Server
  deriving (Eq)

isCurr :: PlayState -> Utils.CurPos -> Bool
isCurr ps cp = (psCur ps) == cp