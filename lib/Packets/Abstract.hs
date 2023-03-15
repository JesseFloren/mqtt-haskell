{-# LANGUAGE LambdaCase #-}
module Packets.Abstract where

import Utils (Bit)
import qualified Data.Map as M

--- *** Packet with Commands *** ---
data Packet  = Packet CommandType Flags Header Payload deriving (Show)
data CommandType = CONNECT
                 | CONNACK
                 | PUBLISH
                 | PUBACK
                 | PUBREC
                 | PUBREL
                 | PUBCOMP
                 | SUBSCRIBE
                 | SUBACK
                 | UNSUBSCRIBE
                 | UNSUBACK
                 | PINGREQ
                 | PINGRESP
                 | DISCONNECT
                 deriving (Ord, Eq, Show)

commandMap :: M.Map CommandType Int
commandMap = M.fromList [(CONNECT, 1), (CONNACK, 2), (PUBLISH, 3), (PUBACK, 4), (PUBREC, 5), (PUBREL, 6), (PUBCOMP, 7),
        (SUBSCRIBE, 8), (SUBACK, 9), (UNSUBSCRIBE, 10), (UNSUBACK, 11), (PINGREQ, 12), (PINGRESP, 13), (DISCONNECT, 14)]

lookupKey :: Eq v => M.Map k v -> v -> [k]
lookupKey m val = M.foldrWithKey go [] m where
  go key value found =
    if value == val
    then key:found
    else found

--- *** Flags *** ---
data QoS      = Zero | One | Two deriving (Eq, Ord, Show)
type Dup      = Bool
type Retain   = Bool
type PacketId = Int
type ClientId = String
type Topic    = String
type SessionPersist = Bool
type KeepAlive = Int

data ConnectFlags = ConnectFlags {
        username :: Maybe String,
        password :: Maybe String,
        will :: Maybe (Retain, QoS, Topic, String),
        cleanSession :: Bool
} deriving (Show)

data ConnackResponse = Accepted | BadProtocalError | BadClientIdError | UnavailableError | BadAuthError | AuthError deriving (Ord, Eq, Show)

mapConnackResponse :: M.Map ConnackResponse Int
mapConnackResponse = M.fromList [(Accepted, 0),
                                (BadProtocalError, 1),
                                (BadClientIdError, 2),
                                (UnavailableError, 3),
                                (BadAuthError, 4),
                                (AuthError, 5)]

data PublishFlags = PublishFlags {
        dup :: Bool,
        retain :: Bool,
        channel :: (Topic, QoS)
} deriving (Show)

putQoS :: QoS -> Int
putQoS = \case {Zero -> 0; One -> 1; Two -> 2}

getQoS :: Int -> QoS
getQoS = \case {1 -> One; 2 -> Two; _ -> Zero}

--- *** Header and Payload Content *** ---
data Content = Str String | Int16 Int | Flags [Bit] | Con Bool | Int8 Int | QoS QoS deriving (Show)
type Header  = [Content]
type Payload = [Content]
type Flags = [Bit]



