{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude

import Board

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick String

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

data Player = Local | Server
  deriving (Eq)

isCurr :: PlayState -> CurPos -> Bool
isCurr ps cp = (psCur s) == c