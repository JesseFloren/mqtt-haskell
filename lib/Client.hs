{-# LANGUAGE RecordWildCards #-}
module Client (clientOpen, clientClose, clientSend, sub, subGroup, Subscription, ClientConfig(..)) where

import Socket.Client
import Network.Socket.ByteString ()
import Packets.Abstract ( Topic, ClientId )
import Control.Concurrent (forkIO, killThread)
import Socket.Base ( createSocket )
import qualified Data.Map as M
import Network.Socket (close)

--- *** Client Functions *** ---
clientOpen :: ClientId -> ClientConfig -> Subscription -> IO MqttClient
clientOpen cid (ClientConfig {..}) s = do
    sock <- createSocket (hostname, portnumber)
    -- TODO-1: Implement handleConnect in Socket.Client
    conResp <- handleConnect sock
    -- TODO-2: Implement handleSubscribe in Socket.Client
    subResp <- handleSubscribe sock quality s
    -- TODO-3: Check conResp before subscribing
    -- TODO-4: Check subResp before opening listenPublish
    thread <- forkIO $ listenPublish sock quality s
    return $ MqttClient cid sock thread (M.keys s)

clientClose :: MqttClient -> IO ()
clientClose (MqttClient {..}) = do
    -- TODO-5: Implement handleUnsubscribe
    unSubResp <- handleUnsubscribe socket subscriptions
    -- TODO-6: Check unSubResp before disconnecting
    _ <- handleDisconnect socket
    killThread thread
    close socket

-- TODO-7: Implement clientSend
clientSend :: ClientSocket -> Topic -> String -> IO ()
clientSend = undefined

--- *** Subscription Building *** ---
subGroup :: [Subscription] -> Subscription
subGroup = M.unions

sub :: Topic -> (ClientSocket -> String -> IO ()) -> Subscription
sub topic callback = M.fromList [(topic, callback)]