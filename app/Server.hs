module Server where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
    ( dupChan, newChan, readChan, writeChan, forkIO, killThread, Chan )
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 24000 0)
  listen sock 2
  matchPlayers sock 1

type Msg = (Int, String)

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


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Int -> IO ()
runConn (sock, _) chan myID opponentID  = do

    let broadcast msg = writeChan chan (myID, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    -- hPutStrLn hdl "Hi, what's your name?"
    name <- hGetLine hdl
    -- something weird with name, need to essentially do (init (hGetLine hdl)) but hGetLine returns Monad, so 
    -- fmap init (hGetLine hdl) is the way to do it. Why is init necessary?
    -- name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ (init name) ++ "!")

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (playerID, line) <- readChan chan
        putStrLn ("reader thread for " ++ (show myID) ++ ": " ++ (show playerID) ++ " " ++ line)
        if (playerID == opponentID) 
          then hPutStrLn hdl line
          else 
            when (playerID /= myID) (putStrLn ("reader thread for " ++ (show myID) ++ ": unexpected message from " ++ (show playerID) ++ " " ++ line ))
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle