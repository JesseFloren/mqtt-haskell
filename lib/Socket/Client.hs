module Socket.Client where

import Network.Socket ( Socket, HostName, PortNumber )
import Network.Socket.ByteString ()
import Packets.Abstract ( Topic, ClientId, Packet (..), QoS, CommandType (..) )
import Control.Concurrent (ThreadId)
import Socket.Base ( recvPacket )
import qualified Data.Map as M

type ClientSocket = Socket
data MqttClient = MqttClient {clientId :: ClientId, socket :: ClientSocket, thread :: ThreadId, subscriptions :: [Topic]}
data ClientConfig = ClientConfig {hostname :: HostName, portnumber :: PortNumber, quality :: QoS}
type Subscription = M.Map Topic (ClientSocket -> String -> IO ())

data Response = Success | Error String

--- *** Creates Connection *** ---
handleConnect :: Socket -> IO Response
handleConnect = undefined

--- *** Creates Subscriptions *** ---
handleSubscribe :: Socket -> QoS -> Subscription -> IO Response
handleSubscribe = undefined

--- *** Handle incomming messages *** ---
listenPublish :: Socket -> QoS -> Subscription -> IO ()
listenPublish sock qos s = do
    packet <- recvPacket sock
    case cmd packet of
        PUBLISH -> undefined
        command -> putStrLn $ "Received packet of type: " ++ show command
    listenPublish sock qos s

--- *** Removes Subscriptions *** ---
handleUnsubscribe :: Socket -> [Topic] -> IO Response
handleUnsubscribe = undefined

--- *** Removes Connection *** ---
handleDisconnect :: Socket -> IO ()
handleDisconnect = undefined