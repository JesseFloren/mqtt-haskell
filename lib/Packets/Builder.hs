module Packets.Builder where
import Data.ByteString.Builder (Builder, int16BE, int8, stringUtf8, toLazyByteString)
import Packets.Abstract
import Data.ByteString (ByteString, toStrict)
import qualified Data.Map as M
import Utils (bitsToInt, intToBits)

type SizedBuilder = (Int, Builder)

cmdBuilder :: CommandType -> Flags -> Builder
cmdBuilder x f = int8 . fromIntegral . bitsToInt $ intToBits 4 (commandMap M.! x) ++ f

contentBuilder :: Content -> SizedBuilder
contentBuilder (Str str)    = (length str + 2, int16BE (fromIntegral $ length str) <> stringUtf8 str)
contentBuilder (Int16 num)  = (2, int16BE $ fromIntegral num)
contentBuilder (Int8 num)   = (1, int8 $ fromIntegral num)
contentBuilder (Con bool)   = (1, int8 (if bool then 1 else 0))
contentBuilder (Flags bits) = (1, int8 $ fromIntegral $ bitsToInt bits)
contentBuilder (QoS qos)    = (1, int8 $ case qos of {Zero -> 0; One -> 1; Two -> 2})

groupSizedBuilder :: SizedBuilder -> SizedBuilder -> SizedBuilder
groupSizedBuilder (s1, x1) (s2, x2) = (s1 + s2, x1 <> x2) 

contentArrBuilder :: [Content] -> SizedBuilder
contentArrBuilder = foldr (groupSizedBuilder . contentBuilder) (0, mempty)

packetToByteString :: Packet -> ByteString
packetToByteString (Packet cmd flags header payload) = toStrict $ toLazyByteString package where
    (size, builder) = contentArrBuilder header `groupSizedBuilder` contentArrBuilder payload
    package = cmdBuilder cmd flags <> int16BE (fromIntegral size) <> builder
