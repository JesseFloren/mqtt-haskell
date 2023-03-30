module Client (open, send, receive, subscribe, close, Connection(..)) where

import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Socket as S
import qualified Socket as Sock
import Packets.Abstract
import Packets.Parser
import Packets.IO (mkPacketIdCounter, PacketIdCounter)
import qualified Packets.Builder as PB
import qualified Packets.Simple as Simple
import Client.Connection 


-- | Opens a connection to a Broker at a specified address 
open :: Sock.SocketAddress -> IO Connection
open addr = Conn <$> connect addr <*> mkPacketIdCounter

-- | Opens a socket at a specified address 
connect :: Sock.SocketAddress -> IO S.Socket
connect addr = do
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  serverAddr <- Sock.createSockAddr addr

  sock <$ S.connect sock serverAddr

type Message = String

-- | Sends a message to the Broker
send :: ConnAction (Message ->  IO ())
send = do
  pkt <- mkMessagePacket 
  pkt `chainM` sendPacket

mkMessagePacket :: ConnAction (Message -> IO Packet)
mkMessagePacket = do
    getPId <- getNextPacketId
    return $ \msg -> do
      pId <- getPId
      return $ Simple.writePublishPacket pId (PublishFlags False False ("", Zero)) msg


-- | Waits for a message to be received
receive :: ConnAction (IO Message)
receive = do
  pkt <- receivePacket
  return (extractMessage <$> pkt)
  where
    -- purposefully naive implementation as the protocol has yet to be described
    extractMessage p | (Just (i, fs, str)) <- Simple.readPublishPacket p = str 

-- | Closes the connection to the Broker gracefully
close :: ConnAction (IO ())
close = do
  sock <- getSock
  -- MQTT-specific actions
  return (S.close sock)

-- | Subscribes the client to a topic
subscribe :: ConnAction (IO ())
subscribe = do
  -- MQTT-specific actions
  returnIO ()

-- TODO specify whether sending was successful
-- | Send a packet to the broker
sendPacket :: ConnAction (Packet -> IO ())
sendPacket = do
  sock <- getSock
  return (sendAll sock . PB.packetToByteString)

receivePacket :: ConnAction (IO Packet)
receivePacket = receiveIO <$> getSock
  where
    receiveIO :: S.Socket -> IO Packet
    receiveIO sock = do
      mPacket <- byteStringToPacket <$> recv sock 1024
      case mPacket of
        Nothing  -> receiveIO sock
        (Just p) -> return p
