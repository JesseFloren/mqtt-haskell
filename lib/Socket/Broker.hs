module Socket.Broker where
import Network.Socket (Socket)
import Control.Concurrent (ThreadId)

type BrokerSocket = Socket
data MqttBroker = MqttBroker {socket :: BrokerSocket, thread :: ThreadId}