{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Utils(Bit(..), bit, bitVal, bitsToInt, intToBits, Byte, BitParser(..), satisfy, parseBit, parseBits, parseInt, parseString, parseSize, parseBool) where
import Control.Applicative (Alternative(..))
import Data.Char (chr)

--- *** BitManipulation base *** ---
data Bit = O | I deriving (Eq)
type Byte = [Bit]

instance Show Bit where
    show :: Bit -> String
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

--- *** Parsing *** ---
newtype BitParser a = BitParser {parse :: [Bit] -> (Maybe a, [Bit])}

instance Functor BitParser where
  fmap :: (a -> b) -> BitParser a -> BitParser b
  fmap f (BitParser g) = BitParser $ \xs -> case g xs of 
        (Nothing, xs') -> (Nothing, xs')
        (Just x', xs') -> (Just $ f x', xs')

instance Applicative BitParser where
  pure :: a -> BitParser a
  pure x = BitParser (Just x,)
  (<*>) :: BitParser (a -> b) -> BitParser a -> BitParser b
  (<*>) (BitParser f) (BitParser g) = BitParser $ \xs -> case f xs of
        (Nothing, xs') -> (Nothing, xs')
        (Just f', xs') -> case g xs' of
            (Nothing, xs'') -> (Nothing, xs'')
            (Just x', xs'') -> (Just $ f' x', xs'')

instance Monad BitParser where
  (>>=) :: BitParser a -> (a -> BitParser b) -> BitParser b
  (>>=) (BitParser g) f = BitParser $ \xs -> case g xs of
        (Nothing, xs') -> (Nothing, xs')
        (Just x', xs') -> let (BitParser f') = f x' in f' xs'

instance Alternative BitParser where
  empty :: BitParser a
  empty = BitParser (Nothing,)
  (<|>) :: BitParser a -> BitParser a -> BitParser a
  (<|>) (BitParser g) (BitParser h) = BitParser $ \xs -> case g xs of
        x'@(Just _, _) -> x'
        _              -> h xs

satisfy :: (Bit -> Bool) -> BitParser Bit
satisfy f = BitParser $ \case {(x:xs) -> if f x then (Just x, xs) else (Nothing, x:xs); xs -> (Nothing, xs)}

parseBit :: Bit -> BitParser Bit
parseBit b = satisfy (== b)

parseBool :: BitParser Bool
parseBool = BitParser $ \case {(I:xs) -> (Just True, xs); (O:xs) -> (Just False, xs); [] -> (Nothing, [])}

parseSize :: Int -> BitParser [Bit]
parseSize 0 = pure []
parseSize c = (:) <$> satisfy (const True) <*> parseSize (c - 1)

parseBits :: [Bit] -> BitParser [Bit]
parseBits = foldr (\x -> (<*>) ((:) <$> parseBit x)) (pure [])

parseInt :: Int -> BitParser Int
parseInt size = BitParser $ \xs -> let (xs1, xs2) = splitAt size xs in 
    if length xs1 == size then (Just $ bitsToInt xs1, xs2) else (Nothing, xs)

parseChar :: BitParser Char
parseChar = chr <$> parseInt 8

parseString :: Int -> BitParser String
parseString 0 = pure []
parseString c = (:) <$> parseChar <*> parseString (c - 1)
