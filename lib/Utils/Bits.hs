module Utils.Bits where

data Bit = O | I deriving (Eq)
type Byte = [Bit]

instance Show Bit where
    show O = "0"
    show I = "1"

bit :: Bool -> Bit
bit True  = I
bit False = O

bitVal :: Bit -> Int -> Int
bitVal O _ = 0
bitVal I i = 1 * 2^(i - 1)

bitsToInt :: [Bit] -> Int
bitsToInt [] = 0
bitsToInt (x:xs) = bitVal x (length xs + 1) + bitsToInt xs

intToBits :: Int -> Int -> [Bit]
intToBits 0 _ = []
intToBits size i = let c = bitVal I size in bit (i >= c):intToBits (size - 1) (if i >= c then i - c else i)