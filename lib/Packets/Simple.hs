{-# LANGUAGE RecordWildCards #-}

module Packets.Simple where

import Packets.Abstract
import Utils (Bit(..), bit)
import Data.Maybe (isJust)

pubFlags :: Dup -> QoS -> Retain -> [Bit]
pubFlags d q r = [bit d, bit (q == Two), bit (q == One), bit r]

emptyFlags :: [Bit]
emptyFlags = [O, O, O, O]

specFlags :: [Bit]
specFlags = [O, O, I, O]

connectMessage :: ClientId -> ConnectFlags -> Int -> Packet
connectMessage cid (ConnectFlags{..}) keepAlive = Packet CONNECT emptyFlags headers payload where
    addOnJust :: [Maybe String] -> [Content] -> [Content]
    addOnJust [] ys = ys
    addOnJust (Just x:xs) ys = addOnJust xs (ys ++ [Str x])
    addOnJust (Nothing:xs) ys = addOnJust xs ys
    headers :: [Content]
    headers = [Str "MQTT", Int8 4, Flags flags, Int16 keepAlive]
    (flags, payload) = case will of
        (Just (wRetain, wQoS, wTopic, wMessage)) -> (f, addOnJust [username, password] [Str cid, Str wTopic, Str wMessage]) where
            f = [bit (isJust username), bit (isJust password), bit wRetain, bit (wQoS == Two), bit (wQoS == One), I, bit cleanSession, O]
        (Nothing) -> (f, addOnJust [username, password] [Str cid]) where
            f = [bit (isJust username), bit (isJust password), O, O, O, O, bit cleanSession, O]

-- 0; 0x00 Connection accepted
-- 1; 0x01 The Server does not support the level of the MQTT protocol requested by the Client
-- 2; 0x02 The Client identifier is correct UTF-8 but not allowed by the Server
-- 3; 0x03 The Network Connection has been made but the MQTT service is unavailable
-- 4; 0x04 The data in the user name or password is malformed 
-- 5; 0x05 The Client is not authorized to connect
connackMessage :: Packet
connackMessage = Packet CONNACK emptyFlags [Con True, Int8 0] []

publishMessage :: PacketId -> Dup -> Retain -> (Topic, QoS) -> String -> Packet --      Dup QoS Retain  Topic             QoS          Message
publishMessage pid dup ret (topic, qos) str = Packet PUBLISH (pubFlags dup qos ret) [Str topic, Int16 pid] [Str str]

pubackMessage :: PacketId -> Packet
pubackMessage pid = Packet PUBACK emptyFlags [Int16 pid] []

pubrecMessage :: PacketId -> Packet
pubrecMessage pid = Packet PUBREC emptyFlags [Int16 pid] []

pubrelMessage :: PacketId -> Packet
pubrelMessage pid = Packet PUBREL specFlags [Int16 pid] []

pubcompMessage :: PacketId -> Packet
pubcompMessage pid = Packet PUBCOMP emptyFlags [Int16 pid] []

subscribeMessage :: PacketId -> [(Topic, QoS)] -> Packet
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