module Packets.Simple where
import Packets.Abstract (Packet(..), CommandType(..), Content(..), QoS (..), Dup, Retain)
import Utils (Bit(..), bit)

pubFlags :: Dup -> QoS -> Retain -> [Bit]
pubFlags d q r = [bit d, bit (q == Two), bit (q == One), bit r]

emptyFlags :: [Bit]
emptyFlags = [O, O, O, O]

specFlags :: [Bit]
specFlags = [O, O, I, O]

connectMessage :: Packet --     Protocal          Version     Connect Flags                        Keep Alive       Complicated payload
connectMessage = Packet CONNECT emptyFlags [Str "MQTT", Int8 4, Flags [O, O, O, O, O, O, O, O], Int16 65535] [Str "PQRST"]

-- 0; 0x00 Connection Accepted; 
-- Connection accepted
-- 1; 0x01 Connection Refused, unacceptable protocol version; 
-- The Server does not support the level of the MQTT protocol requested by the Client
-- 2; 0x02 Connection Refused, identifier rejected; 
-- The Client identifier is correct UTF-8 but not allowed by the Server
-- 3; 0x03 Connection Refused, Server unavailable; 
-- The Network Connection has been made but the MQTT service is unavailable
-- 4; 0x04 Connection Refused, bad user name or password; 
-- The data in the user name or password is malformed 
-- 5; 0x05 Connection Refused, not authorized; 
-- The Client is not authorized to connect
connackMessage :: Packet --     Session Present Response Code
connackMessage = Packet CONNACK emptyFlags [Con True] [Int8 0]

publishMessage :: Packet --      Dup QoS Retain  Topic             QoS          Message
publishMessage = Packet PUBLISH (pubFlags False Two True) [Str "topic", Int16 3] [Str "Hello"]

pubackMessage :: Packet --    Packet Id
pubackMessage  = Packet PUBACK emptyFlags [Int16 3] []

pubrecMessage :: Packet --    Packet Id
pubrecMessage  = Packet PUBREC emptyFlags [Int16 3] []

pubrelMessage :: Packet --    Packet Id
pubrelMessage = Packet PUBREL specFlags [Int16 3] []

pubcompMessage :: Packet --     Packet Id
pubcompMessage = Packet PUBCOMP emptyFlags [Int16 3] []

subscribeMessage :: Packet --       Packet Id    [(Topic,           QoS)   ...]
subscribeMessage = Packet SUBSCRIBE specFlags [Int16 3] [Str "topic", QoS One]

subackMessage :: Packet --    Packet Id    Return Code: {QoS 1: (1, Num 0x01), QoS 2: (1, Num 0x02), Fail: (1, Num 0x80)}
subackMessage = Packet SUBACK emptyFlags [Int16 3] [Int8 0]

ubsubscribeMessage :: Packet --         Packet Id    [(Topic,           QoS)   ...]
ubsubscribeMessage = Packet UNSUBSCRIBE specFlags [Int16 3] [Str "topic", QoS One]

unsubackMessage :: Packet --      Packet Id  
unsubackMessage = Packet UNSUBACK emptyFlags [Int16 3] []

pingreqMessage :: Packet
pingreqMessage = Packet PINGREQ emptyFlags [] []

pingrespMessage :: Packet
pingrespMessage = Packet PINGRESP emptyFlags [] []

disconnectMessage :: Packet
disconnectMessage = Packet DISCONNECT emptyFlags [] []