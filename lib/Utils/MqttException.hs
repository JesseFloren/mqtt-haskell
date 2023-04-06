module Utils.MqttException where
import Control.Exception.Base (Exception)

data MqttException = DisconnectException deriving Show
instance Exception MqttException