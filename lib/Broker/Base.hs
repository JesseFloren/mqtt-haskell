{-# LANGUAGE RecordWildCards #-}

module Broker.Base (MqttBroker(..), createBroker) where

import Network.Socket ( Socket, accept)
import Control.Concurrent
import Control.Exception.Base ( SomeException, try )
import Utils.Socket (createServer, recvPacket, sendPacket)
import Packets
import qualified Data.Map as M
import Data.Either ( isLeft )
import Broker.State
import Broker.Handlers.Response (handleResponse)
import Utils.IO (whenJust, whenJust2)
import Broker.Handlers.Connection ( handleConnect, handleSubscribe )

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId, sThread :: ThreadId }

--- *** Exported Functions *** ---
createBroker :: BrokerConfig -> IO MqttBroker
createBroker conf = do
    sock <- createServer (port conf)
    state <- BrokerState <$> newEmptyMVar <*> newMVar [] <*> pure conf
    sThread <- forkIO $ loopSessionHandler state
    aThread <- forkIO $ loopAcceptClient sock state
    mThread <- forkIO $ loopMessageQueue state
    return $ MqttBroker sock aThread mThread sThread

--- *** Looping Functions *** ---
loopMessageQueue :: BrokerAction ()
loopMessageQueue state = do
    message <- popMessage state
    whenJust message $ \msg -> mapSession (sendMessage msg False) state
    loopMessageQueue state

loopSessionHandler :: BrokerAction ()
loopSessionHandler state = do
    threadDelay (resendDelay (config state) * 1000)
    mapSession_ (\s -> mapM_ (\x -> sendMessage x True s) (pending s)) state
    loopSessionHandler state

loopAcceptClient :: Socket -> BrokerAction ()
loopAcceptClient broker state = do
    (client, _) <- accept broker
    _ <- forkIO $ connectClient client state
    loopAcceptClient broker state

listenToClient :: Socket -> BrokerAction ()
listenToClient sock state = do
    resp <- recvPacket sock
    tid <- myThreadId
    maybe (disconnect sock state >> killThread tid) (\p -> handleResponse tid p sock state) resp
    listenToClient sock state

--- *** Other Functions *** ---
connectClient :: Socket -> BrokerAction ()
connectClient sock state = do
    conResp <- handleConnect sock state
    subResp <- handleSubscribe sock
    whenJust2 (conResp, subResp) $ \(session, subs) -> do
        modifyMVar_ (sessions state) (\x -> return $ mergeSubscriptions session subs:filter (\s -> clientId s /= clientId session) x)
        putStrLn $ "Connected with client: " ++ clientId session
        listenToClient sock state

sendMessage :: Message -> Dup -> Session -> IO Session
sendMessage m@(Message topic msg pid) dup s@(Session {..}) = case (conn, M.member topic subscriptions) of
    (Just sock, True) -> do
        let qos = subscriptions M.! topic
        result <- try (sendPacket sock $ writePublishPacket pid (PublishFlags dup False (topic, qos)) msg) :: IO (Either SomeException ())
        return $ s {pending = if qos /= Zero then m:pending else pending, conn = if isLeft result then Nothing else conn}
    _ -> return s

mergeSubscriptions :: Session -> [(Topic, QoS)] -> Session
mergeSubscriptions s subs =  s {subscriptions = subscriptions s `M.union` M.fromList subs}