{-# LANGUAGE LambdaCase #-}
module Packets.Abstract where

import Utils.Bits ( Bit )
import Packets.CommandType ( CommandType )

--- *** Packet with Commands *** ---
data Packet  = Packet {cmd :: CommandType, flags :: Flags, header :: Header, payload :: Payload} deriving (Show, Eq)

--- *** Flags *** ---
type Flags = [Bit]

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
} deriving (Show, Eq)

data PublishFlags = PublishFlags {
        dup :: Bool,
        retain :: Bool,
        channel :: (Topic, QoS)
} deriving (Show, Eq)

putQoS :: QoS -> Int
putQoS = \case {Zero -> 0; One -> 1; Two -> 2}

getQoS :: Int -> QoS
getQoS = \case {1 -> One; 2 -> Two; _ -> Zero}

--- *** Header and Payload Content *** ---
data Content = Str String | Int16 Int | Flags [Bit] | Con Bool | Int8 Int | QoS QoS deriving (Show, Eq)

-- | Variable Header. Common to most but not all packets
type Header  = [Content]

-- | Present in some but not all packets
type Payload = [Content]



