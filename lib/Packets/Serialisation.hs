{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
module Packets.Serialisation where

import Packets.Abstract
import qualified Data.ByteString as C
import qualified Data.ByteString.Char8 as BC
import Utils.Bits (bitsToInt, bit)
import Data.Bits (shift, (.|.))
import Data.Word 


-- Necessary because we cannot create an instance for the tuple (CommandType, Flags)
data PacketHeader = PHeader CommandType Flags

class Serialisable s where
  serialise :: s -> C.ByteString
  
(<+>) :: (Serialisable a, Serialisable b) => a -> b -> C.ByteString
(<+>) a b = serialise a `C.append` serialise b


instance Serialisable s => Serialisable [s] where
  serialise :: [s] -> C.ByteString
  serialise = C.concat . map serialise

instance Serialisable Packet where
  serialise :: Packet -> C.ByteString
  serialise (Packet cmd flags header payload) = PHeader cmd flags <+> header <+> payload

-- | CommandType and Flags are stored in the same byte, so they have a shared instance
instance Serialisable PacketHeader where
  serialise :: PacketHeader -> C.ByteString
  serialise (PHeader cmd flags) = serialise bits
    where
      bits = (toWord8 cmd `shift` 4) .|. (fromIntegral $ bitsToInt flags)


-- TODO change to Serialisable [Content] to avoid byte wastage
instance Serialisable Content where
  serialise :: Content -> C.ByteString
  serialise (Str s) = BC.pack s -- avoids overlapping instances
  serialise (Int16 i) = serialise i
  serialise (Int8 i) = serialise i
  serialise (Flags fs) = serialise @Int $ bitsToInt fs
  serialise (Con b) = serialise @Int $ bitsToInt [bit b]
  serialise (QoS qos) = serialise @Int $ putQoS qos

instance Serialisable Int where
  serialise :: Int -> C.ByteString
  serialise = C.singleton . fromIntegral

instance Serialisable Word8 where
  serialise :: Word8 -> C.ByteString
  serialise = C.singleton

instance Serialisable C.ByteString where
  serialise :: C.ByteString -> C.ByteString
  serialise = id


