module Broker where

import Network.Socket
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Data.ByteString.Char8 (unpack)

-- | Initialization function. Sets up the environment and shared memory (MVar)
runServer :: IO ()
runServer = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 8000 (tupleToHostAddress (0,0,0,0)))
    putStrLn "Listening"
    listen sock 5
    listMVars <- newMVar []
    acceptLoop sock listMVars

{-|
    This is the heart of the server. Accepting new connections, deciding if they should be publishers or
    subscribers based on the received message. Every client is passed onto its own thread (forkIO).
    Subscribers only have their connection (conn) and a shared memory space.
    Publishers have their connection (conn) and a list with all the shared memory spaces of the subscribers
-}
acceptLoop :: Socket -> MVar [MVar String] -> IO b
acceptLoop sock listMVars = do
    (conn, clientAddr) <- accept sock
    putStrLn $ "Connection from " ++ show clientAddr
    msg <- recv conn 1024
    sendAll conn $ C.pack $ "Server: " ++ unpack msg 
    _ <- createHandler conn (unpack msg)
    acceptLoop sock listMVars

    where
        createHandler conn "pub" = do
            putStrLn "Created pub"
            forkIO $ pubHandler conn listMVars 
        createHandler conn _     = do
            putStrLn "Created sub"
            subMVar <- newEmptyMVar
            list <- takeMVar listMVars
            -- When a new subscriber is added, their shared memory space is added to the list of the publisher
            putMVar listMVars $ list ++ [subMVar]
            forkIO $ subHandler conn subMVar

{-|
    pubHandler receives messages from publishers after which it writes the message in all the shared memory
    spaces of the subscribers. Keep in mind that the list of shared memory spaces itself is also a shared memory
    space (listOfMvars). This is updated by acceptLoop every time a new subscriber joins
    pubHandler could theoretically block, but only when other publishers are writing. 
    Keep in mind this function is called within a thread, so conn is different each call.
-}
pubHandler :: Socket -> MVar [MVar String]-> IO b
pubHandler conn listOfMvars = do
    msg <- recv conn 1024
    putStrLn $ "Received: " ++ show msg
    myList <- takeMVar listOfMvars
    mapM_ (`putMVar` unpack msg) myList
    putMVar listOfMvars myList
    pubHandler conn listOfMvars

{-|
    subHandler is a blocking function. When the share memory space is empty it does nothing but wait untill
    something is put inside it. When something is put inside the shared memory space it is send to the client
    via the conn variable. Keep in mind this function is called within a thread, so conn is different each call.
-}
subHandler :: Socket -> MVar String -> IO b
subHandler conn mVar = do
    result <- takeMVar mVar
    sendAll conn $ C.pack result
    subHandler conn mVar