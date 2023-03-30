module Packets.CommandType(CommandType(..), toWord8, fromWord8) where

import qualified Data.Map as M
import Utils.Map
import Data.Word 

{--
TODO finish this partial documentation
Command types 

PUBLISH
  A PUBLISH packet is sent from a Client to a Server or from a Server to a Client to transport an Application Message.

--}
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

commandMap :: M.Map CommandType Word8
commandMap = M.fromList [
    (CONNECT, 1)
  , (CONNACK, 2)
  , (PUBLISH, 3)
  , (PUBACK, 4)
  , (PUBREC, 5)
  , (PUBREL, 6)
  , (PUBCOMP, 7)
  , (SUBSCRIBE, 8)
  , (SUBACK, 9)
  , (UNSUBSCRIBE, 10)
  , (UNSUBACK, 11)
  , (PINGREQ, 12)
  , (PINGRESP, 13)
  , (DISCONNECT, 14)
  ]

toWord8 :: CommandType -> Word8
toWord8 ct = toB commandMap ct

fromWord8 :: Word8 -> CommandType
fromWord8 i = fromB commandMap i

