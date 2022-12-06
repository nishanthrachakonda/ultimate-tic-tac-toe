{-# LANGUAGE DeriveGeneric #-}

module MsgTypes where

import GHC.Generics
import Data.Serialize
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString
import Data.Char (chr)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C2

--sendMsg :: Bytestring -> IO ()
    



readMsgFromNetwork :: Socket -> IO (Either String GeneralMsg)
readMsgFromNetwork sock = do
                            receivedMsg <- recv sock 1024
                            let msg1 = (decode receivedMsg) :: Either String MsgHeader
                            case msg1 of 
                                Left s -> return (Left ("Error parsing header message: " ++ s))
                                Right msg -> if (msgHeaderType msg /= 1) 
                                               then 
                                                 return (Left ("Header message has wrong type"))
                                               else (readNextMsgFromNetwork  sock (msgType msg) (msgLen msg))

parseBSAsMsgHeader :: ByteString -> IO (Either String MsgHeader)
parseBSAsMsgHeader receivedMsg = 
                                 do
                                    let msg1 = (decode receivedMsg) :: Either String MsgHeader
                                    case msg1 of 
                                        Left s -> return (Left ("Error parsing header message: " ++ s))
                                        Right msg -> if (msgHeaderType msg /= 1) 
                                            then 
                                            return (Left ("Header message has wrong type"))
                                        else return (Right msg)

-- bsToString :: ByteString -> String
-- bsToString = Prelude.map (chr . fromEnum) . C2.unpack


readNByteString :: Socket -> Int -> IO ByteString
readNByteString sock n = do
                            msg <- recv sock 1024
                            let lengthLeft = n - (Data.ByteString.length msg) in 
                                if (lengthLeft <= 0)
                                    then 
                                    return (msg) 
                                    else
                                    do
                                        ioresult <- (readNByteString (sock) (lengthLeft))
                                        return  (msg <> ioresult)


readNextMsgFromNetwork :: Socket -> Int -> Int -> IO (Either String GeneralMsg)
readNextMsgFromNetwork sock msgtype msglen = 
                              do
                                receivedMsg <- readNByteString sock msglen
                                if (msgtype == 2)
                                  then
                                        do
                                         let msg1 = (decode receivedMsg) :: Either String ConnectMsg
                                         case msg1 of 
                                            Left s -> return (Left ("Error parsing header message: " ++ s))
                                            Right msg -> if ((connectMsgType msg) /= 2) 
                                               then 
                                                 return (Left ("Connect message has wrong type"))
                                               else return (Right (ConnMsg msg) )
                                  else   
                                    if (msgtype == 3)
                                      then
                                        do
                                         let msg1 = (decode receivedMsg) :: Either String AckMsg
                                         case msg1 of 
                                            Left s -> return (Left ("Error parsing header message: " ++ s))
                                            Right msg -> if (ackMsgType msg /= 3) 
                                               then 
                                                 return (Left ("Ack message has wrong type"))
                                               else return (Right  (AkMsg msg))
                                       else 
                                         if (msgtype == 4)
                                            then
                                              do
                                                let msg1 = (decode receivedMsg) :: Either String PairedMsg
                                                case msg1 of 
                                                    Left s -> return (Left ("Error parsing header message: " ++ s))
                                                    Right msg -> 
                                                        if (pairedMsgType msg /= 4) then 
                                                            return (Left ("Paired message has wrong type"))
                                                        else return (Right (PdMsg msg))
                                         else 
                                           if (msgtype == 5)
                                            then
                                              do
                                                let msg1 = (decode receivedMsg) :: Either String MoveMsg
                                                case msg1 of 
                                                    Left s -> return (Left ("Error parsing header message: " ++ s))
                                                    Right msg -> 
                                                        if (moveMsgType msg /= 5) then 
                                                            return (Left ("Move message has wrong type"))
                                                        else return (Right (MvMsg msg))
                                             else 
                                               if (msgtype == 6)
                                                 then
                                                   do
                                                     let msg1 = (decode receivedMsg) :: Either String DisconnectMsg
                                                     case msg1 of 
                                                       Left s -> return (Left ("Error parsing header message: " ++ s))
                                                       Right msg -> 
                                                         if (disconnectMsgType msg /= 6) then 
                                                            return (Left ("Disconnect message has wrong type"))
                                                        else return (Right (DcMsg msg))
                                                  else return (Left ("Wrong message type in header"))



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

data GeneralMsg = ConnMsg ConnectMsg | AkMsg AckMsg | PdMsg PairedMsg | MvMsg MoveMsg | DcMsg DisconnectMsg