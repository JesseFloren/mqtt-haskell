{-# LANGUAGE LambdaCase #-}
module Broker where

import Network.Socket
import Control.Concurrent
import Socket.Base (createServer, recvPacket, sendPacket)
import Packets.Abstract ( QoS, Topic )
import Utils.Queue ( Queue )

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId }

data Session = Session {clientId :: String, subscriptions :: [(Topic, QoS)], keepAlive :: Int, conn :: Maybe Socket}

data Message = Message {topic :: Topic, msg :: String}


createBroker :: PortNumber -> IO MqttBroker
createBroker port = do
    sock <- createServer port
    sessions <- newMVar []
    messages <- newEmptyMVar
    aThread <- forkIO $ acceptClientLoop sock sessions
    mThread <- forkIO $ messageQueue messages sessions
    return $ MqttBroker sock aThread mThread


messageQueue :: MVar (Queue Message) -> MVar [Session] -> IO ()
messageQueue = undefined

acceptClientLoop :: Socket -> MVar [Session] -> IO ()
acceptClientLoop broker sessions = do
    (client, _) <- accept broker
    _ <- forkIO $ listenToClient client sessions
    acceptClientLoop broker sessions

listenToClient :: Socket -> MVar [Session] -> IO ()
listenToClient sock sessions = do
    conResp <- handleConnect sock
    subResp <- handleSubscribe sock
    case (conResp, subResp) of
        (Just (Session cid subs ka conn), Just subscriptions) -> do 
            modifyMVar_ sessions $ return . (Session cid (subs ++ subscriptions) ka conn:)
            listenToClient sock sessions
        _ -> return ()

handleConnect :: Socket -> IO (Maybe Session)
handleConnect = undefined

handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe = undefined
