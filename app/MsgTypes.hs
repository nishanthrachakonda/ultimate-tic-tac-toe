{-# LANGUAGE DeriveGeneric #-}

module MsgTypes where

import GHC.Generics
import Data.Serialize


data MsgHeader = MsgHeader {
    msgHeaderType :: Int, -- should always be 1
    msgLen :: Int,
    msgType :: Int -- one of 2,3,4,5,6
} deriving Generic

instance Serialize MsgHeader


-- When client's connect call succeeds, it should send this to the server
data ConnectMsg = ConnectMsg {
    connectMsgType :: Int, -- should always be 2
    connectMsgPlayerName :: String 
} deriving Generic

instance Serialize ConnectMsg

-- Server's response to ConnectMsg
data AckMsg = AckMsg {
    ackMsgType :: Int, -- should be always 3
    ackString :: String,
    playerNum :: Int
} deriving Generic

instance Serialize AckMsg

data PairedMsg = PairedMsg {
    pairedMsgType :: Int, -- should be always 4
    pairMsgOpponentName :: String
} deriving Generic

instance Serialize PairedMsg

data MoveMsg = MoveMsg {
    moveMsgType :: Int, -- should be always 5
    moveGridNum :: Int,
    moveGridRow :: Int,
    moveGridCol :: Int
} deriving Generic

instance Serialize MoveMsg

data DisconnectMsg = DisconnectMsg {
    disconnectMsgType :: Int, -- should be always 6
    disconnectReason :: String
} deriving Generic

instance Serialize DisconnectMsg
