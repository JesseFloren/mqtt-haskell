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
  , token::Maybe Token
}

noAuth :: CID -> Host -> PortNumber -> MqttConfig
noAuth c h p = MqttConfig c h p Nothing