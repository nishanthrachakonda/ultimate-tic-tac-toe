module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))

import Board
import Model
import Utils
import Grid
import Data.Maybe 
import Debug.Trace (trace)

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS s (play s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> continue (move Grid.up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> continue (move Grid.down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue (move Grid.left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> continue (move Grid.right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> halt s
  _                               -> continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Int -> Int) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psCur = moveb f (psCur s) }

-------------------------------------------------------------------------------
play :: PlayState -> (Status, Maybe (GridStatus, Board))
-------------------------------------------------------------------------------
play s = putb (psBoard s) (psPos s) (psCur s) (psTurn s)

-------------------------------------------------------------------------------
nextGameS :: PlayState -> (Status, Maybe (GridStatus, Board)) -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextGameS p s = case s of
  (Error, _)  -> continue p
  (Success, _) -> continue p {psPos = snd (psCur p), psBoard = snd(fromJust (snd s)), psTurn = (Board.flipXO $ psTurn p)}