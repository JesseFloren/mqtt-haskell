{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Client where

import Network.Socket ( close, Socket )
import Network.Socket.ByteString (recv, sendAll)
import Socket.Base (createSocket, sendPacket, recvPacket)
import Packets 
import Control.Concurrent (forkIO)
import Client.Connection (Connection (Conn, sock), ConnAction, getNextPacketId, chainM, getSock, apply)
import Client.MqttConfig (MqttConfig(..))
import Client.Subscription (Subscription, topics, findHandler)
import Data.Maybe(maybe)
import qualified Packets.Simple as Simple
import qualified Data.Set as S


open :: MqttConfig -> Subscription -> IO Connection
open conf subs = do
    sock <- createSocket (host conf, port conf)
    handleConnect sock conf
    handleSubscribe sock (topics subs)
    putStrLn "Connected with broker successfully"
    conn <- Conn sock <$> mkPacketIdCounter
    _ <- forkIO $ listenToServer conn subs
    return conn

handleConnect :: Socket -> MqttConfig -> IO ()
handleConnect sock conf = do
    sendPacket sock $ writeConnectPacket (cid conf) (ConnectFlags (token conf) (token conf) Nothing False) 60000
    connack <- recvPacket sock >>= (\case {Just x -> return $ readConnackPacket x; Nothing -> return Nothing})
    case connack of
        Just (_, Accepted) -> return ()
        a -> error $ "Failed to connect " ++ show a

handleSubscribe :: Socket -> S.Set Topic -> IO ()
handleSubscribe sock topics = do
    let pkt = writeSubscribePacket 0 $ S.toList (S.map (,Zero) topics)
    sendPacket sock pkt
    suback <- maybe Nothing readSubackPacket <$> recvPacket sock
    case suback of
        Just (_, _) -> return ()
        _ -> error "Failed to subscribe"

--- *** Send to Broker *** ---
send :: ConnAction ((Topic, String) -> IO ())
send = do
  pkt <- mkMessagePacket
  pkt `chainM` (sendPacket <$> getSock)

mkMessagePacket :: ConnAction ((Topic, String) -> IO Packet)
mkMessagePacket = do
    getPId <- getNextPacketId
    return $ \(topic,msg) -> do
      pId <- getPId
      return $ writePublishPacket pId (PublishFlags False False (topic, Zero)) msg


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
close = do Network.Socket.close <$> getSock

--- *** Listen to Server *** ---
-- | Continuously listens to server specified in Connection
listenToServer :: Connection -> Subscription -> IO ()
listenToServer conn subs = do
    response <- recvPacket (sock conn)
    case response of
      Nothing -> return ()
      Just packet -> listenToServer conn subs <* handlePacket conn subs packet

-- | Handles received packets
handlePacket :: Connection -> Subscription -> Packet -> IO ()
handlePacket conn subs pack 
  | PUBLISH <- cmd pack = handlePublish conn subs pack
  | otherwise = return ()

handlePublish :: Connection -> Subscription -> Packet -> IO ()
handlePublish conn subs packet 
  | Just (_, flags, message) <- readPublishPacket packet = (($ message) <$> subs `findHandler` fst (channel flags)) `apply` conn
  | otherwise = return ()
