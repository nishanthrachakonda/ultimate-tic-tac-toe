{-# LANGUAGE OverloadedStrings #-}

module Client where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Char (isDigit)
import Data.Serialize


-- module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C2
import qualified Data.ByteString.Lazy as C
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (chr)
-- import Network.Socket
import Network.Socket.ByteString (recv, sendAll, send)
import Data.String (String)
import MsgTypes
import Data.ByteString (ByteString, length)

main :: IO ()
main = runTCPClient "127.0.0.1" "24000" $ \s -> do
    putStrLn "Hi, what's your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")
    let playerMsg = (encode (ConnectMsg {connectMsgType = 2, connectMsgPlayerName = name}))
    let hdrMsg = (encode (MsgHeader 1 (Data.ByteString.length playerMsg) 2))
    putStrLn (show (Data.ByteString.length hdrMsg))
    send s hdrMsg
    send s playerMsg
    repeatingProcessIncoming s (-1)
    putStrLn "Disconnected. Thank you for playing !!"

repeatingProcessIncoming :: Socket -> Int -> IO ()
repeatingProcessIncoming s playerNum = do
  actualMsg <- readMsgFromNetwork s
  case actualMsg of 
    Left s -> (putStrLn ("Unable to read the message, received a Left -> " ++ s)) >> return ()
    Right (AkMsg msg) ->  do
                            playerNum2 <- handleACK msg s playerNum
                            repeatingProcessIncoming s playerNum2
    Right (PdMsg msg) -> (handleStart msg s playerNum) >> (repeatingProcessIncoming s playerNum)
    Right (MvMsg msg) -> (handleMove msg s playerNum) >> (repeatingProcessIncoming s playerNum)
    Right (DcMsg msg) -> (handleDisconnect msg s playerNum)
    otherwise       -> (putStrLn "Unable to read the msg received, you might've sent a connect msg by mistake ?") >> return()
  return ()


handleDisconnect :: DisconnectMsg -> Socket -> Int -> IO ()
handleDisconnect msg s playerNum = do 
  putStrLn ("Disconnecting, recieved the following message from the server - " ++ (disconnectReason msg))
  return ()

handleACK :: AckMsg -> Socket -> Int -> IO Int
handleACK msg s playerNumb = do
  putStrLn "Connected to the server."
  putStrLn "Waiting for 2 players to join."
  putStrLn ("The server says : " ++ (ackString msg))
  putStrLn ("You are player number " ++ (show (playerNum msg)))
  return (playerNum msg)

handleStart :: PairedMsg -> Socket -> Int -> IO ()
handleStart msg s playerNum = do
  putStrLn "We have 2 players ready and can now begin the game."
  putStrLn ("You are facing off against " ++ (pairMsgOpponentName msg) ++ ". Have fun !")
  return ()

handleMove :: MoveMsg -> Socket -> Int -> IO ()
handleMove msg s playerNum = do
  putStrLn "Your opponent has made the following move."

  --These two sections are TODO
  ---------------------------------
  --Print the grid after the opponents move.
  putStrLn "GRID GRID GRID GRID GRID GRID"
  ---------------------------------


  ---------------------------------
  --get the userinput for the move to be made.
  let (gridNumber, x, y) = (1, 1, 1)
  ---------------------------------


  let myMove = (encode (MoveMsg {moveMsgType = 5, moveGridNum = gridNumber, moveGridRow = x, moveGridCol = y}))
  send s myMove
  return ()


-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError ((\addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock


-- main :: IO ()
-- main = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   bind sock (SockAddrInet 4242 0)
--   listen sock 2
--   chan <- newChan
--   _ <- forkIO $ fix $ \loop -> do
--     (_, _) <- readChan chan
--     loop
--   mainLoop sock chan 0

-- type Msg = (Int, String)

-- mainLoop :: Socket -> Chan Msg -> Int -> IO ()
-- mainLoop sock chan msgNum = do
--   conn <- accept sock
--   forkIO (runConn conn chan msgNum)
--   mainLoop sock chan $! msgNum + 1

-- runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
-- runConn (sock, _) chan msgNum = do
--     let broadcast msg = writeChan chan (msgNum, msg)
--     hdl <- socketToHandle sock ReadWriteMode
--     hSetBuffering hdl NoBuffering

--     hPutStrLn hdl "Hi, what's your name?"
--     name <- fmap init (hGetLine hdl)
--     broadcast ("--> " ++ name ++ " entered chat.")
--     hPutStrLn hdl ("Welcome, " ++ name ++ "!")

--     commLine <- dupChan chan

--     -- fork off a thread for reading from the duplicated channel
--     reader <- forkIO $ fix $ \loop -> do
--         (nextNum, line) <- readChan commLine
--         when (msgNum /= nextNum) $ hPutStrLn hdl line
--         loop

--     handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
--         line <- fmap init (hGetLine hdl)
--         case line of
--              -- If an exception is caught, send a message and break the loop
--              "quit" -> hPutStrLn hdl "Bye!"
--              -- else, continue looping.
--              _      -> broadcast (name ++ ": " ++ line) >> loop

--     killThread reader                      -- kill after the loop ends
--     broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
--     hClose hdl                             -- close the handle