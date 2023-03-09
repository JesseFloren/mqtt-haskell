{-# LANGUAGE LambdaCase #-}

module Packets.Parser (byteStringToPacket) where

--- *** Imports *** ---
import Utils ( intToBits, BitParser (parse), parseInt, parseString, parseSize )
import Data.ByteString (ByteString)
import qualified Data.ByteString as C
import qualified Data.Map as M
import Control.Applicative (empty, Alternative (some))
import Packets.Abstract

--- *** Get Key from map with Value *** ---
lookupKey :: Eq v => M.Map k v -> v -> [k]
lookupKey m val = M.foldrWithKey go [] m where
  go key value found =
    if value == val
    then key:found
    else found

--- *** Parser *** ---
parseCmd :: BitParser CommandType
parseCmd = head . lookupKey commandMap <$> parseInt 4

parseStr :: BitParser Content
parseStr = Str <$> (parseInt 16 >>= parseString)

parseCon :: BitParser Content
parseCon = parseInt 8 >>= \case {0 -> pure (Con False); 1 -> pure (Con True); _ -> empty}

parseInt8 :: BitParser Content
parseInt8 = Int8 <$> parseInt 8

parseInt16 :: BitParser Content
parseInt16 = Int16 <$> parseInt 16

parseQoS :: BitParser Content
parseQoS = parseInt 8 >>= \case {0 -> pure (QoS Zero); 1 -> pure (QoS One); 2 -> pure (QoS Two); _ -> empty}

parseFlags :: Int -> BitParser Content
parseFlags count = Flags <$> parseSize count

parseVariableHeader :: CommandType -> BitParser Header
parseVariableHeader CONNACK    = (\sp res -> [sp, res]) <$> parseCon <*> parseInt8
parseVariableHeader PUBLISH    = (\t pid -> [t, pid]) <$> parseStr <*> parseInt16
parseVariableHeader PINGREQ    = pure []
parseVariableHeader PINGRESP   = pure []
parseVariableHeader DISCONNECT = pure []
parseVariableHeader _          = (: []) <$> parseInt16

parsePayload :: CommandType -> BitParser Payload
parsePayload CONNECT     = undefined
parsePayload PUBLISH     = (: []) <$> parseStr
parsePayload CONNACK     = pure []
parsePayload SUBACK      = (: []) <$> parseInt8
parsePayload SUBSCRIBE   = concat <$> some ((\t q -> [t, q]) <$> parseStr <*> parseQoS)
parsePayload UNSUBSCRIBE = concat <$> some ((\t q -> [t, q]) <$> parseStr <*> parseQoS)
parsePayload _           = pure []

parsePacket :: BitParser Packet
parsePacket = parseCmd >>= \cmd -> Packet cmd <$> parseSize 4 <* parseInt16 <*> parseVariableHeader cmd <*> parsePayload cmd

--- *** Exposed functions *** ---
byteStringToPacket :: ByteString -> Maybe Packet
byteStringToPacket bs = case C.unpack bs of
        [] -> Nothing
        xs -> fst $ parse parsePacket bits where
            bits = concatMap (intToBits 8 . fromIntegral) xs