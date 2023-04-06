{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Client where

import Network.Socket ( close, Socket )
import Network.Socket.ByteString (recv)
import Socket.Base (createSocket, sendPacket, recvPacket)
import Packets
import Control.Concurrent (forkIO, killThread, myThreadId, throwTo, newMVar, threadDelay, readMVar)
import Utils.MqttException
import Client.Connection (Connection (..), ConnAction, getNextPacketId, chainM, getSock, apply, getConn, removeFromPending, addToPending, readPending)
import Client.MqttConfig (MqttConfig(..))
import Client.Subscription (Subscription, getHandler, getSubs)
import qualified Packets.Simple as Simple
import qualified Data.Map as M
import Control.Monad

open :: MqttConfig -> Subscription -> IO Connection
open conf subs = do
    sock <- createSocket (host conf, port conf)
    handleConnect sock conf
    handleSubscribe sock (getSubs subs)
    putStrLn "Connected with broker successfully"
    conn <- Conn sock <$> mkPacketIdCounter <*> newMVar []
    mainId <- myThreadId
    pendingId <- forkIO $ handlePending (conn []) (resendDelay conf)
    listenId <- forkIO $ listenToServer (conn [mainId, pendingId]) subs
    return (conn [listenId, pendingId])

handlePending :: Connection -> Int -> IO ()
handlePending conn ms = do
    threadDelay (ms * 1000)
    readPending `apply` conn >>= mapM_ (\(pid, (topic, msg)) -> sendPacket (sock conn) $ writePublishPacket pid (PublishFlags False False (topic, One)) msg)
    handlePending conn ms

handleConnect :: Socket -> MqttConfig -> IO ()
handleConnect sock conf = do
    sendPacket sock $ writeConnectPacket (cid conf) (ConnectFlags (token conf) (token conf) Nothing False) 60000
    connack <- recvPacket sock >>= (\case {Just x -> return $ readConnackPacket x; Nothing -> return Nothing})
    case connack of
        Just (_, Accepted) -> return ()
        a -> error $ "Failed to connect " ++ show a

handleSubscribe :: Socket -> M.Map Topic QoS -> IO ()
handleSubscribe sock subs = do
    sendPacket sock $ writeSubscribePacket 0 $ M.toList subs
    suback <- recvPacket sock >>= (\case {Just x -> return $ readSubackPacket x; Nothing -> return Nothing})
    case suback of
        Just (_, _) -> return ()
        _ -> error "Failed to subscribe"

--- *** Send to Broker *** ---
send :: QoS -> ConnAction ((Topic, String) -> IO ())
send qos = do
  pkt <- mkMessagePacket qos
  pkt `chainM` (sendPacket <$> getSock)

mkMessagePacket :: QoS -> ConnAction ((Topic, String) -> IO Packet)
mkMessagePacket qos = do
    getPId <- getNextPacketId
    addPend <- addToPending
    return $ \(topic,msg) -> do
      pId <- getPId
      when (qos == One) $ addPend (pId, (topic, msg))
      return $ writePublishPacket pId (PublishFlags False False (topic, qos)) msg


-- | Waits for a message to be received
receive :: ConnAction (IO String)
receive = do
  pkt <- receivePacket
  return (extractMessage <$> pkt)
  where
    -- purposefully naive implementation as the protocol has yet to be described
    extractMessage p | (Just (i, fs, str)) <- Simple.readPublishPacket p = str

receivePacket :: ConnAction (IO Packet)
receivePacket = receiveIO <$> getSock
  where
    receiveIO :: Socket -> IO Packet
    receiveIO sock = do
      mPacket <- byteStringToPacket <$> recv sock 1024
      case mPacket of
        Nothing  -> receiveIO sock
        (Just p) -> return p

--- *** Close client *** ---
close :: ConnAction (IO ())
close = close' <$> getConn

close' :: Connection -> IO ()
close' conn = do
  sendPacket (sock conn) writeDisconnectPacket
  mapM_ killThread (threads conn) -- Listening thread dies here
  Network.Socket.close (sock conn)

--- *** Listen to Server *** ---
listenToServer :: Connection -> Subscription -> IO ()
listenToServer conn subs = do
    response <- recvPacket (sock conn)
    case response of
        Nothing -> return ()
        Just packet -> do
            case cmd packet of
                PUBLISH -> do
                    handlePublish conn subs packet
                    listenToServer conn subs
                PUBACK -> do
                    maybe (return ()) (removeFromPending `apply` conn) (readPacketId packet)
                    listenToServer conn subs
                DISCONNECT -> do
                  throwTo (head $ threads conn) DisconnectException
                  return () -- Thread dies here
                _ -> listenToServer conn subs

handlePublish :: Connection -> Subscription -> Packet -> IO ()
handlePublish conn subs packet = do
    case readPublishPacket packet of
        Nothing -> return ()
        Just (pid, flags, message) -> do
          when (snd (channel flags) == One) $ sendPacket (sock conn) $ writePubackPacket pid
          (($ message) <$> subs `getHandler` fst (channel flags)) `apply` conn
