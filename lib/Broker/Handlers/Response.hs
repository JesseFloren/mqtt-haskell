{-# LANGUAGE RecordWildCards #-}

module Broker.Handlers.Response where

import Broker.State
import Control.Concurrent
import Network.Socket ( close )
import Packets
import Utils.Socket ( sendPacket )
import Control.Monad ( when )

handleResponse :: ThreadId -> BrokerHandler
handleResponse tid packet sock state = case cmd packet of
    PUBLISH -> handlePublish packet sock state
    PUBACK -> handlePuback packet sock state
    DISCONNECT -> handleDisconnect tid packet sock state
    _ -> return ()

handlePublish :: BrokerHandler 
handlePublish p sock state = case readPublishPacket p of
    Nothing -> return ()
    Just (pid, PublishFlags _ _ (topic, qos), msg) -> do
        when (qos == One) $ sendPacket sock $ writePubackPacket pid
        pushMessage (Message topic msg pid) state

handlePuback :: BrokerHandler 
handlePuback packet sock (BrokerState{..}) = do
    modifyMVar_ sessions $ mapM $ \s@(Session {..}) -> (
        if Just sock == conn then do
            return $ Session clientId subscriptions keepAlive will [msg | msg <- pending, readPacketId packet /= Just (pid msg)] conn listenThread
        else return s)

handleDisconnect :: ThreadId -> BrokerHandler
handleDisconnect tid _ sock (BrokerState{..})= do
    session <- head <$> (filter (\s -> conn s == Just sock) <$> readMVar sessions)
    putStrLn $ "Received close from: " ++ clientId session
    modifyMVar_ sessions (return . filter (\s -> conn s /= Just sock))
    close sock
    killThread tid