{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Packets.Simple where

import Packets.Abstract
import Utils
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Map as M

pubFlags :: Dup -> QoS -> Retain -> [Bit]
pubFlags d q r = [bit d, bit (q == Two), bit (q == One), bit r]

emptyFlags :: [Bit]
emptyFlags = [O, O, O, O]

specFlags :: [Bit]
specFlags = [O, O, I, O]

--- *** Write Packets *** ---
writeConnectPacket :: ClientId -> ConnectFlags -> KeepAlive -> Packet
writeConnectPacket cid (ConnectFlags{..}) keepAlive = Packet CONNECT emptyFlags headers payload where
    flags :: Maybe (Retain, QoS, Topic, String) -> [Bit]
    flags (Just (wRetain, wQoS, _, _)) =
        [bit (isJust username), bit (isJust password), bit wRetain, bit (wQoS == Two), bit (wQoS == One), I, bit cleanSession, O]
    flags Nothing =
        [bit (isJust username), bit (isJust password), O, O, O, O, bit cleanSession, O]

    headers :: [Content]
    headers = [Str "MQTT", Int8 4, Flags (flags will), Int16 keepAlive]

    payload :: [Content]
    payload = maybe [Str cid] mapWill will ++ mapMaybe (Str<$>) [username, password] where
        mapWill :: (Retain, QoS, Topic, String) -> [Content]
        mapWill (_, _, wTopic, wMessage) = [Str cid, Str wTopic, Str wMessage]


writeConnackPacket :: SessionPersist -> ConnackResponse -> Packet
writeConnackPacket sp code = Packet CONNACK emptyFlags [Con sp, Int8 (mapConnackResponse M.! code)] []

writePublishPacket :: PacketId -> PublishFlags -> String -> Packet
writePublishPacket pid (PublishFlags{..}) str =
    Packet PUBLISH (pubFlags dup (snd channel) retain) [Str (fst channel), Int16 pid] [Str str]

writePubackPacket :: PacketId -> Packet
writePubackPacket pid = Packet PUBACK emptyFlags [Int16 pid] []

writePubrecPacket :: PacketId -> Packet
writePubrecPacket pid = Packet PUBREC emptyFlags [Int16 pid] []

writePubrelPacket :: PacketId -> Packet
writePubrelPacket pid = Packet PUBREL specFlags [Int16 pid] []

writePubcompPacket :: PacketId -> Packet
writePubcompPacket pid = Packet PUBCOMP emptyFlags [Int16 pid] []

writeSubscribePacket :: PacketId -> [(Topic, QoS)] -> Packet
writeSubscribePacket pid topics = Packet SUBSCRIBE specFlags [Int16 pid] (subTopics topics) where
    subTopics :: [(Topic, QoS)] -> [Content]
    subTopics = concatMap (\(str, qos) -> [Str str, QoS qos])

writeSubackPacket :: PacketId -> [Maybe QoS] -> Packet
writeSubackPacket pid qos = Packet SUBACK emptyFlags [Int16 pid] (map (Int8 . maybe 0x80 putQoS) qos)

writeUnsubscribePacket :: PacketId -> [(Topic, QoS)] -> Packet
writeUnsubscribePacket pid topics = Packet UNSUBSCRIBE specFlags [Int16 pid] (subTopics topics) where
    subTopics :: [(Topic, QoS)] -> [Content]
    subTopics = concatMap (\(str, qos) -> [Str str, QoS qos])

writeUnSubackPacket :: PacketId -> Packet
writeUnSubackPacket pid = Packet UNSUBACK emptyFlags [Int16 pid] []

writePingreqPacket :: Packet
writePingreqPacket = Packet PINGREQ emptyFlags [] []

writePingrespPacket :: Packet
writePingrespPacket = Packet PINGRESP emptyFlags [] []

writeDisconnectPacket :: Packet
writeDisconnectPacket = Packet DISCONNECT emptyFlags [] []

--- *** Read Packets *** ---
findContent :: (Content -> Maybe a) -> [Content] -> Maybe a
findContent _ [] = Nothing
findContent f (x:xs) = case f x of {(Just x') -> Just x'; Nothing -> findContent f xs}

maybeTup2 :: (Maybe a, Maybe b) -> Maybe (a, b)
maybeTup2 (Just x, Just y) = Just (x, y)
maybeTup2 _ = Nothing

maybeTup3 :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
maybeTup3 (Just x, Just y, Just z) = Just (x, y, z)
maybeTup3 _ = Nothing

readPacketId :: Packet -> Maybe PacketId
readPacketId (Packet _ _ header _) = findContent (\case {(Int16 pid) -> Just pid; _ -> Nothing}) header

readConnectPacket :: Packet -> Maybe (ClientId, ConnectFlags, KeepAlive)
readConnectPacket (Packet _ _ [Str "MQTT", Int8 4, Flags flags, Int16 keepAlive] payload) = fst flags' >>= pack payload where
    flags' = parse ((,,,,,) <$> parseBool <*> parseBool <*> parseBool <*> (getQoS <$> parseInt 2) <*> parseBool <*> parseBool) flags
    pack :: [Content] -> (Bool, Bool, Bool, QoS, Bool, Bool)-> Maybe (ClientId, ConnectFlags, KeepAlive)
    pack [Str clientId, Str wTopic, Str wMessage, Str user, Str pass] (True, True, ret, qos, True, cs) =
        Just (clientId, ConnectFlags (Just user) (Just pass) (Just (ret, qos, wTopic, wMessage)) cs, keepAlive)
    pack [Str clientId, Str wTopic, Str wMessage] (False, False, ret, qos, True, cs) =
        Just (clientId, ConnectFlags Nothing Nothing (Just (ret, qos, wTopic, wMessage)) cs, keepAlive)
    pack [Str clientId, Str user, Str pass] (True, True, _, _, False, cs) =
        Just (clientId, ConnectFlags (Just user) (Just pass) Nothing cs, keepAlive)
    pack [Str clientId] (False, False, _, _, False, cs) =
        Just (clientId, ConnectFlags Nothing Nothing Nothing cs, keepAlive)
    pack _ _ = Nothing
readConnectPacket _ = Nothing


readConnackPacket :: Packet -> Maybe (SessionPersist, ConnackResponse)
readConnackPacket (Packet _ _ header _) = maybeTup2 (sp, resp) where
    sp = findContent (\case {(Con v) -> Just v; _ -> Nothing}) header
    resp = findContent (\case {(Int8 v) -> head' $ mapConnackResponse `lookupKey` v; _ -> Nothing}) header
    head' [] = Nothing
    head' (x:_) = Just x

readPublishPacket :: Packet -> Maybe (PacketId, PublishFlags, String)
readPublishPacket p@(Packet _ [dup, qos2, qos1, ret] header payload) = maybeTup3 (pid, flags, msg) where
    flags = case channel of {(Just c) -> Just PublishFlags{dup= dup==I, retain= ret==I, channel=c}; Nothing -> Nothing}
    channel = maybeTup2 (topic, qos)
    pid = readPacketId p
    topic = findContent (\case {(Str t) -> Just t; _ -> Nothing}) header
    qos = getQoS <$> case bitsToInt [qos2, qos1] of {3 -> Nothing; x -> Just x}
    msg = findContent (\case {(Str m) -> Just m; _ -> Nothing}) payload
readPublishPacket _ = Nothing

readSubscribePacket :: Packet -> Maybe (PacketId, [(Topic, QoS)])
readSubscribePacket p@(Packet _ _ _ payload) = maybeTup2 (pid, topics payload) where
    pid = readPacketId p
    topics :: [Content] -> Maybe [(Topic, QoS)]
    topics [] = Just []
    topics (Str t:QoS q:xs) = ((t, q):) <$> topics xs
    topics _ = Nothing

readSubackPacket :: Packet -> Maybe (PacketId, Maybe QoS)
readSubackPacket p@(Packet _ _ _ payload) =  maybeTup2 (pid, (getQoS <$>) <$> qos) where
    pid = readPacketId p
    qos = findContent (\case {(Int8 0x80) -> Just Nothing; (Int8 v) -> Just (Just v); _ -> Nothing}) payload

readUnsubscribePacket :: Packet -> Maybe (PacketId, [(Topic, QoS)])
readUnsubscribePacket = readSubscribePacket
