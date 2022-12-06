{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import Utils

import Board

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------
  
data PlayState = PS
  { psX      :: Player   -- ^ player R info
  , psO      :: Player   -- ^ player B info
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Utils.Value     -- ^ whose turn 
  , psCur    :: Utils.CurPos    -- ^ current cursor
  }

init :: PlayState
init = PS {
    psX = Local,
    psO = Server,
    psBoard = Board.init,
    psTurn = X,
    psCur = (1, 1)
}

data Player = Local | Server
  deriving (Eq)

isCurr :: PlayState -> Utils.CurPos -> Bool
isCurr ps cp = (psCur ps) == cp