module Server where

import Network.Socket
import System.IO
    ( hClose,
      hSetBuffering,
      hGetLine,
      hPutStrLn,
      BufferMode(NoBuffering),
      IOMode(ReadWriteMode) )
import Control.Exception ( SomeException(SomeException), handle )
import Control.Concurrent
    ( dupChan, newChan, readChan, writeChan, forkIO, killThread, Chan )
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Serialize
import MsgTypes
import Network.Socket.ByteString
import Data.ByteString (ByteString, length)
import qualified Codec.Binary.UTF8.Generic as Data.Bytestring

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 24000 0)
  listen sock 2
  matchPlayers sock 1

type ChanMsg = (Int, ByteString)
-- data Msg = ConnectMsg | AckMsg | PairedMsg | MoveMsg | DisconnectMsg

-- playerName :: Socket ->   -> IO ()


matchPlayers :: Socket -> Int -> IO ()
matchPlayers sock playerID = do
  -- Accepts 2 connection requests and 
  -- Matches 2 players, and their gameplay is forked into another thread 
  connPlayerOne <- accept sock
  chanPlayerOne <- newChan
  _ <- forkIO (runConn connPlayerOne chanPlayerOne (playerID) (playerID+1))
  connPlayerTwo <- accept sock
  chanPlayerTwo <- dupChan chanPlayerOne
  _ <- forkIO (runConn connPlayerTwo chanPlayerTwo (playerID + 1) (playerID))

  -- Match the next 2 players and so on
  matchPlayers sock (playerID+2)


processMsgHeader :: Int -> ByteString -> IO (Either String Int)
processMsgHeader playerID bstring = do 
                                   result <- parseBSAsMsgHeader bstring
                                   case result of 
                                    Left s -> return (Left "ERROR")
                                    Right msg -> return (Right (msgType msg))

processMsg :: Int -> Int -> Socket -> ByteString -> Bool -> IO ()
processMsg opponentID msgtype sock bstring ismyself = do 
                                   result <- parseBSAsGeneralMsg bstring msgtype
                                   case result of 
                                    Left s -> putStrLn ("ERROR " ++ s)
                                    Right msg -> processMsgHelper sock opponentID msg ismyself

processMsgHelper :: Socket -> Int -> GeneralMsg -> Bool -> IO ()
processMsgHelper sock playerID (ConnMsg a) ismyself = 
  
                                                      if (ismyself)
                                                        then 
                                                          do
                                                              let turn = 2 - (playerID `mod` 2)
                                                              let msg = encode (AckMsg 3 ("Welcome, player " ++ show turn) turn)
                                                              let header = encode (MsgHeader 1 (Data.ByteString.length msg) 3)
                                                              send sock header
                                                              send sock msg
                                                              if (turn == 2)
                                                                then 
                                                                  do 
                                                                    let msg2 = encode (PairedMsg 4 "")
                                                                    let header2 = encode (MsgHeader 1 (Data.Bytestring.length msg2) 4)
                                                                    send sock header2
                                                                    send sock msg2
                                                                    return ()        
                                                                    
                                                                else return ()
                                                        else 
                                                          do
                                                             let msg2 = encode (PairedMsg 4 "")
                                                             let header2 = encode (MsgHeader 1 (Data.Bytestring.length msg2) 4)
                                                             send sock header2
                                                             send sock msg2
                                                             return ()        

processMsgHelper sock playerID (AkMsg a) ismyself = do 
                                                      return ()

processMsgHelper sock playerID (PdMsg a) ismyself = do 
                                                      return ()
processMsgHelper sock playerID (MvMsg a) ismyself = if (ismyself == False)
                                                        then 
                                                          do
                                                              let msg = encode (a)
                                                              let header = encode (MsgHeader 1 (Data.ByteString.length msg) 5)
                                                              send sock header
                                                              send sock msg
                                                              return ()
                                                          else return ()
processMsgHelper sock playerID (DcMsg a) ismyself  = do
                                                        return ()


getSpecificMessage :: Int -> Chan ChanMsg -> IO (ByteString)
getSpecificMessage opponentID chan = do
                                        (plid, realmsg) <- readChan chan
                                        if (plid == opponentID)
                                          then return (realmsg)
                                        else getSpecificMessage opponentID chan


runConn :: (Socket, SockAddr) -> Chan ChanMsg -> Int -> Int -> IO ()
runConn (sock, _) chan myID opponentID  = do

    let broadcast msg = writeChan chan (myID, msg)

    reader <- forkIO $ fix $ \loop -> do
        (playerID, msg) <- readChan chan
        putStrLn ("reader thread for " ++ (show myID) ++ ": " ++ (show playerID) ++ " ")
        if (playerID == opponentID) 
          then 
             do 
               header <- (processMsgHeader opponentID msg) 
               case header of
                Left e -> putStrLn e 
                Right msgtype -> 
                    do 
                      realmsg <- (getSpecificMessage opponentID chan)
                      (processMsg playerID msgtype sock realmsg False)

          else 
            if (playerID == myID)
              then 
                do 
                  header <- (processMsgHeader myID msg) 
                  case header of
                    Left e -> putStrLn e 
                    Right msgtype -> 
                      do 
                        realmsg <- (getSpecificMessage myID chan)
                        (processMsg playerID msgtype sock realmsg True)

            else (putStrLn ("reader thread for " ++ (show myID) ++ ": unexpected message from " ++ (show playerID) ++ " "))
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        res <- readAllFromNetwork sock
        case res of
          Left a -> putStrLn ("Error reading from network: " ++ a)
          Right (hdr, msg) -> do
                                broadcast hdr
                                broadcast msg >> loop

    killThread reader                      -- kill after the loop ends
    --broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    --hClose hdl                             -- close the handle