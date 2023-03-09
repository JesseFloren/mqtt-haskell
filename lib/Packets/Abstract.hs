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

--- *** Flags *** ---
data QoS      = Zero | One | Two deriving (Eq, Ord, Show)
type Dup      = Bool
type Retain   = Bool
type PacketId = Int
type Topic    = String

mapQoS :: QoS -> Int
mapQoS = \case {Zero -> 0; One -> 1; Two -> 2}

--- *** Header and Payload Content *** ---
data Content = Str String | Int16 Int | Flags [Bit] | Con Bool | Int8 Int | QoS QoS deriving (Show)
type Header  = [Content]
type Payload = [Content]
type Flags = [Bit]



