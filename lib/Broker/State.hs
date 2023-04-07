{-# LANGUAGE NamedFieldPuns #-}

module Broker.State where

import Control.Concurrent ( ThreadId, MVar, takeMVar, putMVar, isEmptyMVar, readMVar, modifyMVar_ )
import Packets.Abstract ( Packet, PacketId, QoS, Retain, Topic )
import qualified Data.Map as M
import Network.Socket ( Socket, PortNumber )
import Utils.Queue ( Queue(..), pop, push, single )
import Control.Monad

type AuthToken = Maybe String

type BrokerAction a = BrokerState -> IO a
type BrokerHandler = Packet -> Socket -> BrokerAction ()

data BrokerState = BrokerState {messages :: MVar (Queue Message), sessions :: MVar [Session], config :: BrokerConfig}

data BrokerConfig = BrokerConfig {port :: PortNumber, sSecret :: AuthToken, resendDelay :: Int} 

data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will :: Maybe (Retain, QoS, Topic, String),
    pending :: [Message],
    conn :: Maybe Socket,
    listenThread :: ThreadId} deriving (Show, Eq)

data Message = Message {topic :: Topic, msg :: String, pid :: PacketId} deriving (Show, Eq)

popMessage :: BrokerAction (Maybe Message)
popMessage BrokerState{messages} = do
    (message, queue) <- pop <$> takeMVar messages
    when (queue /= End) $ putMVar messages queue
    return message

pushMessage :: Message -> BrokerAction ()
pushMessage msg BrokerState{messages} = do
    isEmpty <- isEmptyMVar messages
    if isEmpty then putMVar messages (single msg)
    else takeMVar messages >>= putMVar messages . push msg

mapSession :: (Session -> IO Session) -> BrokerAction ()
mapSession f BrokerState{sessions} = modifyMVar_ sessions (mapM f)


mapSession_ :: (Session -> IO a) -> BrokerAction ()
mapSession_ f BrokerState{sessions} = readMVar sessions >>= mapM_ f

disconnect :: Socket -> BrokerAction ()
disconnect sock = mapSession (\s -> return $ s {conn = if conn s == Just sock then Nothing else conn s})