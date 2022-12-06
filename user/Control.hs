module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))

import Board
import Model
import Utils
import Grid

-------------------------------------------------------------------------------

control :: Model.PlayState -> BrickEvent n Model.Tick -> EventM n (Next Model.PlayState)
control s ev = case ev of 
  -- T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS s =<< liftIO (play s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> continue (move Grid.up    (psCur s))
  T.VtyEvent (V.EvKey V.KDown _)  -> continue (move Grid.down  (psCur s))
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue (move Grid.left  (psCur s))
  T.VtyEvent (V.EvKey V.KRight _) -> continue (move Grid.right (psCur s))
  T.VtyEvent (V.EvKey V.KEsc _)   -> halt s
  _                               -> continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Int -> Int) -> Model.PlayState -> Model.PlayState
-------------------------------------------------------------------------------
move f s = s { psCur = moveb f (psCur s) }

-------------------------------------------------------------------------------
play :: Model.PlayState -> Utils.Status Board.Board
-------------------------------------------------------------------------------
play s = put (psBoard s) X (psCur s)

-- -------------------------------------------------------------------------------
-- nextGameS :: Model.PlayState -> Utils.Status -> EventM n (Next Model.PlayState)
-- -------------------------------------------------------------------------------
-- nextGameS p s r = case r of
--   Utils.Error  -> continue s { state = (Play p) }
--   Utils.Success _ -> continue s { state = (Play $ p {psBoard = b, psTurn = (Board.flipXO $ psTurn p)}) }