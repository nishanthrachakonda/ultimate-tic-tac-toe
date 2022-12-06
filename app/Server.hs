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
import MsgTypes (ConnectMsg, AckMsg, MoveMsg, DisconnectMsg)
import Data.ByteString (ByteString)

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


runConn :: (Socket, SockAddr) -> Chan ChanMsg -> Int -> Int -> IO ()
runConn (sock, _) chan myID opponentID  = do

    let broadcast msg = writeChan chan (myID, msg)
    --hdl <- socketToHandle sock ReadWriteMode
    --hSetBuffering hdl NoBuffering
    
    
    --name <- hGetLine hdl
    -- broadcast ("--> " ++ name ++ " entered chat.")
    -- hPutStrLn hdl ("Welcome, " ++ (init name) ++ "!")

    reader <- forkIO $ fix $ \loop -> do
        (playerID, line) <- readChan chan
        --putStrLn ("reader thread for " ++ (show myID) ++ ": " ++ (show playerID) ++ " " ++ line)
        --if (playerID == opponentID) 
        --  then hPutStrLn hdl line
        --  else 
        --    when (playerID /= myID) (putStrLn ("reader thread for " ++ (show myID) ++ ": unexpected message from " ++ (show playerID) ++ " " ++ line ))
        loop

    --handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        --line <- fmap init (hGetLine hdl)
        --case line of
             -- If an exception is caught, send a message and break the loop
        --     "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
        --     _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    -- broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    -- hClose hdl                             -- close the handle