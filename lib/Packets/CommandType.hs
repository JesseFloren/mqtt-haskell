module Packets.CommandType(CommandType(..), commandToInt, intToCommand) where

import qualified Data.Map as M
import Utils.Map ( toB, fromB )

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

commandToInt :: CommandType -> Int
commandToInt = toB commandMap

intToCommand :: Int -> CommandType
intToCommand = fromB commandMap

