module Client (open, send, receive, subscribe, close, Connection(..), SocketAddress(..), IPAddress(..), Port(..), socketAddress) where

import Network.Socket (addrFlags, getAddrInfo, defaultHints, addrAddress, defaultProtocol)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as S

type Connection = S.Socket

newtype IPAddress = IP String
newtype Port = Port Int
data SocketAddress = SocketAddress IPAddress Port

socketAddress :: String -> Int -> SocketAddress
socketAddress ip port = SocketAddress (IP ip) (Port port)

-- | Opens a connection to a Broker at a specified address 
open :: SocketAddress -> IO Connection
open addr = do
  -- Create socket
  sock <- S.socket S.AF_INET S.Stream  defaultProtocol
  serverAddr <- createSocketAddress addr
  
  -- Connect socket to address
  S.connect sock serverAddr

  return sock
    where 
      createSocketAddress :: SocketAddress -> IO S.SockAddr
      createSocketAddress (SocketAddress (IP ip) (Port port)) = do
        info <- getAddrInfo 
                  (Just defaultHints { addrFlags = [S.AI_ADDRCONFIG] }) 
                  (Just ip) 
                  (Just (show port)) 
        return (head $ map addrAddress info)


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

