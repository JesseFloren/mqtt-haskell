module Broker where

import Network.Socket
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString.Char8 as C



runServer :: IO ()
runServer = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 8000 (tupleToHostAddress (0,0,0,0)))
    listen sock 5
    acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (conn, clientAddr) <- accept sock
    putStrLn $ "Connection from " ++ show clientAddr
    handleConnection conn
    acceptLoop sock

handleConnection :: Socket -> IO ()
handleConnection conn = do
    msg <- recv conn 1024
    putStrLn $ "Received: " ++ show msg
    sendAll conn $ C.pack "Thanks for the message!\n"
    close conn
