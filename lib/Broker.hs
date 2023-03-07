module Broker where

import Network.Socket
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString.Char8 as C
import Control.Concurrent


runServer :: IO ()
runServer = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 8000 (tupleToHostAddress (0,0,0,0)))
    putStrLn "Listening"
    listen sock 5
    acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (conn, clientAddr) <- accept sock
    putStrLn $ "Connection from " ++ show clientAddr
    id <- forkIO $ handleConnection conn
    putStrLn $ "Created session: " ++ show id
    acceptLoop sock

handleConnection :: Socket -> IO ()
handleConnection conn = do
    msg <- recv conn 1024
    putStrLn $ "Received: " ++ show msg
    sendAll conn $ C.pack "Thanks for the message!\n"
    handleConnection conn
