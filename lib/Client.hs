module Client where

import Network.Socket ( close, Socket )
import Network.Socket.ByteString (recv)
import Utils.Socket (createSocket, sendPacket, recvPacket)
import Packets
import Control.Concurrent (forkIO, killThread, myThreadId, throwTo, newMVar, threadDelay)
import Utils.MqttException
import Client.Connection (Connection (..), ConnAction, getNextPacketId, chainM, getSock, apply, getConn, removeFromPending, addToPending, readPending)
import Client.MqttConfig (MqttConfig(..))
import Client.Subscription (Subscription, getHandler, getSubs)
import Control.Monad
import qualified Packets.Simple as Simple
import qualified Data.Map as M
import Utils.IO


-- | Open a connection to the Broker and subscribe to the specified Topics
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
    let pkt = writeConnectPacket (cid conf) (ConnectFlags (token conf) (token conf) Nothing False) 60000
    sendPacket sock pkt
    connack <- maybe Nothing readConnackPacket <$> recvPacket sock
    case connack of
        Just (_, Accepted) -> return ()
        a -> error $ "Failed to connect " ++ show a

handleSubscribe :: Socket -> M.Map Topic QoS -> IO ()
handleSubscribe sock subs = do
    let pkt = writeSubscribePacket 0 $ M.toList subs
    sendPacket sock pkt
    suback <- maybe Nothing readSubackPacket <$> recvPacket sock
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


-- | Receive and receivepacket arent used in practice since subsciptions exist
receive :: ConnAction (IO String)
receive = do
  pkt <- receivePacket
  return (extractMessage <$> pkt)
  where
    -- purposefully naive implementation as the protocol has yet to be described
    extractMessage p | (Just (_, _, str)) <- Simple.readPublishPacket p = str
                     | otherwise = undefined

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
-- | Continuously listens to server specified in Connection
listenToServer :: Connection -> Subscription -> IO ()
listenToServer conn subs = do
    response <- recvPacket (sock conn)
    case response of
      Nothing -> return ()
      Just packet -> do
        continue <- handlePacket conn subs packet
        when continue (listenToServer conn subs) -- Thread dies here if continue is False

-- | Handles received packets
handlePacket :: Connection -> Subscription -> Packet -> IO Bool
handlePacket conn subs pack =
  case cmd pack of
    PUBLISH -> True <$ handlePublish conn subs pack
    PUBACK -> True <$ maybe (return ()) (removeFromPending `apply` conn) (readPacketId pack)
    DISCONNECT -> False <$ throwTo (head $ threads conn) DisconnectException
    _ -> return True -- continue listening if packet left unhandled

handlePublish :: Connection -> Subscription -> Packet -> IO ()
handlePublish conn subs packet 
  | Just (pId, flags, message) <- readPublishPacket packet = do
    let pkt = writePubackPacket pId
    when (snd (channel flags) == One) $ sendPacket (sock conn) pkt
    (($ message) <$> subs `getHandler` fst (channel flags)) `apply` conn
  | otherwise = return ()
          
