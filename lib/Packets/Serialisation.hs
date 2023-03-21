module Packets.Serialisation where

import Packets.Abstract
import qualified Data.ByteString as C
import Data.Bits 
import Data.Bits.Extras


class Serialisable a where
  serialise :: a -> C.ByteString

instance Serialisable Packet where
  serialise :: Packet -> C.ByteString
  serialise (Packet cmd flags header payload) = 
    serialise cmd `C.append` serialise flags `C.append` serialise header `C.append` serialise payload

-- | CommandType and Flags are stored in the same byte, so they have a shared instance
instance Serialisable (CommandType, Flags) where
  serialise :: (CommandType, Flags) -> C.ByteString
  serialise (cmd, flags) = serialise bits
    where
      bits = (toWord8 cmd `shift` 4) .|. (bitsToInt flags)

instance Serialisable Int where
  serialise :: Int -> C.ByteString
  serialise = C.singleton . fromIntegral