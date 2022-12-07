module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View 
import Control 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Client
import MsgTypes

-------------------------------------------------------------------------------
main :: IO ()
main =  Client.runTCPClient "127.0.0.1" "24000" (\sock -> do
  -- rounds <- fromMaybe defaultRounds <$> getRounds
  chan   <- newBChan 10
  -- Send connect request to server
  
  Client.sendConnectMsg sock ""
  -- msg <- readMsgFromNetwork sock
  forkIO  $ forever $ do 
    msg <- readMsgFromNetwork sock
    writeBChan chan msg

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  --msg <- ...
  let initState = Model.init { psConn = sock }
  customMain initialVty buildVty (Just chan) app initState
  print ("Game exited")
  )

app :: App PlayState (Either String GeneralMsg) String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])
  }