module Client.MqttConfig (MqttConfig(..), noAuth, CID) where

import Network.Socket ( PortNumber )

-- TODO add documentation to explain acronym
type CID = String 
type Host = String
type Token = String

data MqttConfig = MqttConfig {
    cid::CID
  , host::Host
  , port::PortNumber
  , resendDelay :: Int
  , token::Maybe Token
}

noAuth :: CID -> Host -> PortNumber -> Int -> MqttConfig
noAuth c h p r = MqttConfig c h p r Nothing 