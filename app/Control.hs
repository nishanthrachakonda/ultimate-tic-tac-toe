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
import qualified Brick.BorderMap as Map
-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n (Either String GeneralMsg) -> EventM n (Next PlayState)
control s ev = case ev of 
  T.AppEvent (Left errorString)   ->  continue s
  T.AppEvent (Right (AkMsg msg))  ->  do 
                                        let turn = playerNum msg
                                        if (turn == 1) 
                                          then (continue s {psTurn = X, psPlayerNum = 1, psMessage = "Waiting for another player to join"})
                                        else (continue s {psTurn = O, psPlayerNum = 2, psMessage = "Waiting for another player to join"})
  T.AppEvent (Right (PdMsg msg))  ->  do 
                                          -- Print message below the grid
                                        if (psPlayerNum s == 1) 
                                          then (continue s {psIsMyTurn = 1, psMessage = "You have been paired. Your turn now"})
                                        else (continue s {psMessage = "You have been paired. You move second. Waiting for opponent"})
  T.AppEvent (Right (MvMsg msg))  ->  do 
                                        let grid = (moveGridNum msg)
                                        let x = (moveCellNum msg)
                                        let newstate = s {psIsMyTurn = 1, psCur = (grid,x), psMessage = "Opponent moved. Your turn now"}
                                        let y = putb (psBoard newstate) (psPos newstate) (psCur newstate) (Board.flipXO (psTurn newstate))
                                        case fst y of 
                                          Success -> do
                                                      if (getbS (snd(fromJust (snd y))) /= Ongoing) 
                                                        then continue newstate {psIsMyTurn = 0, psGameState = getbS (snd(fromJust (snd y))), psPos = x, psBoard = snd(fromJust (snd y)) , psMessage = "Game ended" }
                                                      else continue newstate {psGameState = getbS (snd(fromJust (snd y))),  psPos = x, psBoard = snd(fromJust (snd y))}
                                          otherwise -> do
                                                        continue (s {psMessage = "Invalid move from opponent"})
                                        

  T.AppEvent (Right (DcMsg msg))  ->  do 
                                        halt s 
  T.VtyEvent (V.EvKey V.KEnter _) ->  do

                                        if (psIsMyTurn s == 1)
                                          then do
                                                let moveStatus = play s
                                                case fst moveStatus of 
                                                  Success ->  do
                                                                liftIO $ sendMoveMsgWithHeader (psConn s) (psCur s)
                                                                nextGameS (s {psMessage = "Move sent. Waiting for opponent"}) (moveStatus)
                                                  otherwise ->  do
                                                                  nextGameS (s {psMessage = "Invalid move. Try again"})  (moveStatus)
                                        else continue (s {psMessage = "Not your turn"})

  T.VtyEvent (V.EvKey V.KUp   _)  ->  do
                                        if (psIsMyTurn s == 1)
                                          then continue (move Grid.up    s)
                                        else continue (s {psMessage = "Not your turn"})
  T.VtyEvent (V.EvKey V.KDown _)  ->  do
                                        if (psIsMyTurn s == 1)
                                          then continue (move Grid.down    s)
                                        else continue (s {psMessage = "Not your turn"})
  T.VtyEvent (V.EvKey V.KLeft _)  ->  do
                                        if (psIsMyTurn s == 1)
                                          then continue (move Grid.left    s)
                                        else continue (s {psMessage = "Not your turn"})
  T.VtyEvent (V.EvKey V.KRight _) ->  do
                                        if (psIsMyTurn s == 1)
                                          then continue (move Grid.right    s)
                                        else continue (s {psMessage = "Not your turn"})
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
  (Success, Just (Ongoing, _) ) -> continue p {psGameState = getbS (snd(fromJust (snd s))), psPos = snd (psCur p), psBoard = snd(fromJust (snd s)), psTurn = (psTurn p), psIsMyTurn = 0}
  (Success, _ ) -> continue p {psGameState = getbS (snd(fromJust (snd s))), psPos = snd (psCur p), psBoard = snd(fromJust (snd s)), psTurn = (psTurn p), psIsMyTurn = 0, psMessage = "Game ended"}