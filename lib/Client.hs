module Client (open, send, receive, subscribe, close, Connection(..)) where

import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as S
import qualified Socket as Sock

type Connection = S.Socket

-- | Opens a connection to a Broker at a specified address 
open :: Sock.SocketAddress -> IO Connection
open addr = do
  -- Create socket
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  serverAddr <- Sock.createSockAddr addr
  
  -- Connect socket to address
  S.connect sock serverAddr

  return sock

type Message = String

-- | Sends a message to the Broker
send :: Message -> Connection -> IO ()
send msg sock = sendAll sock $ C.pack msg

-- | Waits for a message to be received
receive :: Connection -> IO Message
receive sock = C.unpack <$> recv sock 1024

-- | Closes the connection to the Broker gracefully
close :: Connection -> IO ()
close = do
  -- MQTT-specific actions
  S.close 


-- | Subscribes the client to a topic
subscribe :: Connection -> IO ()
subscribe conn = do
  -- MQTT-specific actions
  return ()

