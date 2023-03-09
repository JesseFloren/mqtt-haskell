module Packets.Simple where

import Packets.Abstract (Packet(..), CommandType(..), Content(..), QoS (..), Dup, Retain, PacketId, Topic, mapQoS)
import Utils (Bit(..), bit)

pubFlags :: Dup -> QoS -> Retain -> [Bit]
pubFlags d q r = [bit d, bit (q == Two), bit (q == One), bit r]

emptyFlags :: [Bit]
emptyFlags = [O, O, O, O]

specFlags :: [Bit]
specFlags = [O, O, I, O]

connectMessage :: Packet --     Protocal          Version     Connect Flags                        Keep Alive       Complicated payload
connectMessage = Packet CONNECT emptyFlags [Str "MQTT", Int8 4, Flags [O, O, O, O, O, O, O, O], Int16 65535] [Str "ClientId"]

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
connackMessage :: Packet
connackMessage = Packet CONNACK emptyFlags [Con True, Int8 0] []

publishMessage :: PacketId -> Dup -> Retain -> (Topic, QoS) -> String -> Packet --      Dup QoS Retain  Topic             QoS          Message
publishMessage pid dup ret (topic, qos) str = Packet PUBLISH (pubFlags dup qos ret) [Str topic, Int16 pid] [Str str]

pubackMessage :: PacketId -> Packet --    Packet Id
pubackMessage pid = Packet PUBACK emptyFlags [Int16 pid] []

pubrecMessage :: PacketId -> Packet --    Packet Id
pubrecMessage pid = Packet PUBREC emptyFlags [Int16 pid] []

pubrelMessage :: PacketId -> Packet --    Packet Id
pubrelMessage pid = Packet PUBREL specFlags [Int16 pid] []

pubcompMessage :: PacketId -> Packet --     Packet Id
pubcompMessage pid = Packet PUBCOMP emptyFlags [Int16 pid] []

subscribeMessage :: PacketId -> [(Topic, QoS)] -> Packet --       Packet Id    [(Topic,           QoS)   ...]
subscribeMessage pid topics = Packet SUBSCRIBE specFlags [Int16 pid] (subTopics topics) where
    subTopics :: [(Topic, QoS)] -> [Content]
    subTopics = concatMap (\(str, qos) -> [Str str, QoS qos])

subackMessage :: PacketId -> Maybe QoS -> Packet
subackMessage pid Nothing    = Packet SUBACK emptyFlags [Int16 pid] [Int8 0x80]
subackMessage pid (Just qos) = Packet SUBACK emptyFlags [Int16 pid] [Int8 (mapQoS qos)]

ubsubscribeMessage :: PacketId -> [(Topic, QoS)] -> Packet
ubsubscribeMessage pid topics = Packet UNSUBSCRIBE specFlags [Int16 pid] (subTopics topics) where
    subTopics :: [(Topic, QoS)] -> [Content]
    subTopics = concatMap (\(str, qos) -> [Str str, QoS qos])

unsubackMessage :: PacketId -> Packet 
unsubackMessage pid = Packet UNSUBACK emptyFlags [Int16 pid] []

pingreqMessage :: Packet
pingreqMessage = Packet PINGREQ emptyFlags [] []

pingrespMessage :: Packet
pingrespMessage = Packet PINGRESP emptyFlags [] []

disconnectMessage :: Packet
disconnectMessage = Packet DISCONNECT emptyFlags [] []