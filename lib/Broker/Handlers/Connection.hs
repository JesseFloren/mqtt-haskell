{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Broker.Handlers.Connection where

import Network.Socket ( close, Socket )
import Broker.State hiding (will)
import qualified Data.Map as M
import Packets
import Control.Concurrent ( myThreadId, killThread, readMVar, ThreadId )
import Utils.Socket ( sendPacket, recvPacket )
import Broker.Auth ( authCheck )
import Utils.IO ( whenJust )
import Control.Applicative ((<|>))
import Data.List (find)

recvDefPacket :: (Packet -> Maybe a) -> Socket -> IO (Maybe a)
recvDefPacket f sock =  recvPacket sock >>= (\case {Just x -> return $ f x; Nothing -> return Nothing})

handleConnect :: Socket -> BrokerAction (Maybe Session)
handleConnect sock state = do
    connectPacket <- recvDefPacket readConnectPacket sock
    let resp = validatePacket connectPacket state
    (sp, session) <- trySession sock resp connectPacket state
    sendPacket sock $ writeConnackPacket sp resp
    return session

validatePacket :: Maybe (ClientId, ConnectFlags, KeepAlive) -> BrokerState -> ConnackResponse
validatePacket m BrokerState{..} = maybe BadProtocalError (\(_, flags, _) -> authCheck (sSecret config) (password flags)) m

trySession :: Socket -> ConnackResponse -> Maybe (ClientId, ConnectFlags, KeepAlive) -> BrokerAction (SessionPersist, Maybe Session)
trySession sock Accepted (Just x@(cid,_,_)) state = do
    currSession <- find (\s -> clientId s == cid) <$> readMVar (sessions state)
    createSession sock x currSession
trySession _ _ _ _ = return (False, Nothing)

createSession :: Socket -> (ClientId, ConnectFlags, KeepAlive) -> Maybe Session -> IO (SessionPersist, Maybe Session)
createSession sock (cid, ConnectFlags _ _ will False, keepAlive) (Just (Session _ subs _ w pending oldSock tid)) = do
    discardSocket oldSock tid
    currThread <- myThreadId
    return (True, Just (Session cid subs keepAlive (w <|> will) pending (Just sock) currThread))
createSession sock (cid, ConnectFlags _ _ will _, keepAlive) _ = do
    currThread <- myThreadId
    return (False, Just (Session cid M.empty keepAlive will [] (Just sock) currThread))

discardSocket :: Maybe Socket -> ThreadId -> IO ()
discardSocket sock tid = do
    killThread tid
    whenJust sock (\s -> s `sendPacket` writeDisconnectPacket >> close s)

handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe sock = do
    subscriptions <- recvDefPacket readSubscribePacket sock
    whenJust subscriptions $ \(pid, subs) -> sendPacket sock $ writeSubackPacket pid (map (Just . snd) subs)
    return $ snd <$> subscriptions
