{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Client where

import Network.Socket ( close, Socket, PortNumber )
import Socket.Base (createSocket, sendPacket, recvPacket)
import Packets.Simple (writeConnectPacket, readConnackPacket, writeSubscribePacket, readSubackPacket, writePublishPacket, readPublishPacket)
import Packets.Abstract (ConnectFlags(ConnectFlags), ConnackResponse (..), QoS (Zero), PublishFlags (PublishFlags, channel), Packet (cmd), CommandType (..), Topic, ClientId)
import Control.Concurrent (forkIO)
import Socket.Client (Connection (Conn, sock), ConnAction, getNextPacketId, chainM, getSock, apply)
import Packets.IO ( mkPacketIdCounter )
import qualified Data.Map as M

data MqttConfig = MqttConfig {cid::String, host::String, port::PortNumber, auth::Maybe (String, String)}
type Subscription = M.Map Topic (ConnAction (String -> IO ()))

runClient :: MqttConfig -> Subscription -> IO Connection
runClient conf subs = do
    sock <- createSocket (host conf, port conf)
    handleConnect sock (cid conf)
    handleSubscribe sock (M.keys subs)
    putStrLn "Connected with broker successfully"
    conn <- Conn sock <$> mkPacketIdCounter
    _ <- forkIO $ listenToServer conn subs
    return conn

handleConnect :: Socket -> ClientId -> IO ()
handleConnect sock cid = do
    sendPacket sock $ writeConnectPacket cid (ConnectFlags Nothing Nothing Nothing False) 60000
    connack <- recvPacket sock >>= (\case {Just x -> return $ readConnackPacket x; Nothing -> return Nothing})
    case connack of
        Just (_, Accepted) -> return ()
        _ -> error "Failed to connect"

handleSubscribe :: Socket -> [Topic] -> IO ()
handleSubscribe sock topics = do
    sendPacket sock $ writeSubscribePacket 0 (map (,Zero) topics)
    suback <- recvPacket sock >>= (\case {Just x -> return $ readSubackPacket x; Nothing -> return Nothing})
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

--- *** Close client *** ---
close :: ConnAction (IO ())
close = do Network.Socket.close <$> getSock

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
                _ -> listenToServer conn subs

handlePublish :: Connection -> Subscription -> Packet -> IO ()
handlePublish conn subs packet = do
    case readPublishPacket packet of
        Nothing -> return ()
        Just (_, flags, message) -> (($ message) <$> subs M.! fst (channel flags)) `apply` conn

--- *** Build Subs *** ---
sub :: Topic -> ConnAction (String -> IO ()) -> Subscription
sub t f = M.fromList [(t,f)]

subGroup :: [Subscription] -> Subscription
subGroup = foldr M.union M.empty