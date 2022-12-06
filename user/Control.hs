module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))

import Board


-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS s =<< liftIO (play s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Int -> Int) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psCol = f (psCol s) }

-------------------------------------------------------------------------------
play :: PlayState -> Result Board
-------------------------------------------------------------------------------
play s = put (psBoard s) X (psCol s)

-- -------------------------------------------------------------------------------
-- playServer :: PlayState -> Int -> Result Board
-- -------------------------------------------------------------------------------
-- playServer s n = case psTurn s of
--   R -> case psR s of
--     Server -> put (psBoard s) R n
--     Local  -> Retry
--   B -> case psB s of
--     Server -> put (psBoard s) B n
--     Local  -> Retry

-------------------------------------------------------------------------------
nextGameS :: PlayState -> Status -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextGameS p s r = case r of
  Error  -> continue s { state = (Play p) }
  Success _ -> continue s { state = (Play $ p {psBoard = b, psTurn = (flipXO $ psTurn p)}) }
--   Draw   -> continue s { state = (initEndMenu 0) } -- Draw is 0
--   Win R  -> continue s { state = (initEndMenu 1) } -- Red win is 1
--   Win B  -> continue s { state = (initEndMenu 2) } -- Blue win is 2, other player left so you win is -1