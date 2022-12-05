{-# LANGUAGE OverloadedStrings #-}

module Client where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Char (isDigit)


-- module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C2
import qualified Data.ByteString.Lazy as C
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (chr)
-- import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.String (String)

main :: IO ()
main = runTCPClient "127.0.0.1" "4242" $ \s -> do
    putStr "Connected: "

    msg <- recv s 1024


    let msg1 = filter isDigit (bsToString (C.fromStrict msg))
    let msg2 = filter (isNotDigit) (bsToString (C.fromStrict msg))  
    let stringLength  = read (msg1) :: Int
    let stringLength2 = length (msg2)
    reqString <- loop1 (toInteger (stringLength - stringLength2)) s
    let msg3 = stringToBS (msg2 ++ reqString)
    putStr "Received: "
    C2.putStrLn (C.toStrict msg3)
    -- display waiting 

readMessage :: Socket -> IO String
readMessage s = do{
  
}

loop1 :: Integer -> Socket -> IO String
loop1 = \lengthRequired s -> do
  msg <- recv s 1024
  putStr (show lengthRequired)
  let msgString = (bsToString (C.fromStrict msg)) in 
    let lengthLeft = lengthRequired - (toInteger (length msgString)) in 
      if (lengthLeft <= 0)
        then 
          return (bsToString (C.fromStrict msg)) 
        else
          do
            ioresult <- (loop1 (lengthLeft) (s))
            return ((bsToString (C.fromStrict msg)) ++ ioresult)


isNotDigit :: Char -> Bool
isNotDigit c = not (isDigit c)


bsToString :: C.ByteString -> String
bsToString = map (chr . fromEnum) . C.unpack

stringToBS :: String -> C.ByteString
stringToBS str = BLU.fromString str



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