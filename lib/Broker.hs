{-# LANGUAGE RecordWildCards #-}
module Broker where

import Network.Socket hiding (socket)
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Data.ByteString.Char8 (unpack)
import Socket.Base (createServer, recvPacket)
import Socket.Broker (MqttBroker (..))
import Packets (Packet(..), CommandType (..))

--- *** Broker Functions *** ---
brokerOpen :: IO MqttBroker
brokerOpen = do
    sock <- createServer 8000
    thread <- forkIO $ connectionHandler sock
    return $ MqttBroker sock thread

brokerClose :: MqttBroker -> IO ()
brokerClose (MqttBroker{..}) = do
    killThread thread
    close socket

--- *** Broker Handlers *** ---
connectionHandler :: Socket -> IO ()
connectionHandler sock = do
    (conn, _) <- accept sock
    _ <- forkIO $ listenClient conn 
    connectionHandler sock

listenClient :: Socket -> IO ()
listenClient sock = do
    -- Handle connection
    -- Handle subscription
    -- Listen publish fork
    -- On publish set MVar for topic


-- subHandler :: Socket -> MVar String -> IO b
-- subHandler conn mVar = do
--     result <- takeMVar mVar
--     sendAll conn $ C.pack result
    subHandler conn mVar