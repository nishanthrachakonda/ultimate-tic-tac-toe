module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))

import Client
import Board
import Model
import Utils
import Grid
import Data.Maybe 
import Debug.Trace (trace)
import MsgTypes
-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n (Either String GeneralMsg) -> EventM n (Next PlayState)
control s ev = case ev of 
  T.AppEvent (Left errorString)   ->  continue s
  T.AppEvent (Right (AkMsg msg))  ->  do 
                                        --update state
                                        (continue s)
  T.AppEvent (Right (PdMsg msg))  ->  do 
                                        --update state
                                        (continue s)
  T.AppEvent (Right (MvMsg msg))  ->  do 
                                        let grid = (moveGridNum msg)
                                        let x = (moveGridRow msg)
                                        let y = (moveGridCol msg)
                                        continue s 

  T.AppEvent (Right (DcMsg msg))  ->  do 
                                        halt s 
  T.VtyEvent (V.EvKey V.KEnter _) ->  do
                                        let moveStatus = play s
                                        case fst moveStatus of 
                                          Success ->  do
                                                        liftIO $ sendMoveMsgWithHeader (psConn s) (psCur s)
                                                        nextGameS s (moveStatus)
                                          otherwise ->  do
                                                          nextGameS s (moveStatus)

  T.VtyEvent (V.EvKey V.KUp   _)  ->  continue (move Grid.up    s)
  T.VtyEvent (V.EvKey V.KDown _)  ->  continue (move Grid.down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  ->  continue (move Grid.left  s)
  T.VtyEvent (V.EvKey V.KRight _) ->  continue (move Grid.right s)
  T.VtyEvent (V.EvKey V.KEsc _)   ->  halt s
  _                               ->  continue s -- Brick.halt s

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