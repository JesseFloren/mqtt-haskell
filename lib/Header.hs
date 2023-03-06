module Header where
import Data.ByteString (pack)

type QoS    = Int
type Dup    = Bool
type Retain = Bool
type Size   = Int

data Header = CONNECT 
            | CONNACK 
            | PUBLISH Dup QoS Retain
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

data Message = Message Header Size String

data Bit = O | I

instance Show Bit where
    show O = "0"
    show I = "1"

getBitValue :: Bit -> Int -> Int
getBitValue O _ = 0
getBitValue I i = 1 * 2^(i - 1)


bitsToInt :: [Bit] -> Int
bitsToInt [] = 0
bitsToInt (x:xs) = getBitValue x (length xs + 1) + bitsToInt xs



