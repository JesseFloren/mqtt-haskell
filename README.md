# MQTT-Haskell
## Description
This library provides an MQTT implementation for Haskell per the [specification of the OASIS MQTT Technical Committe](https://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html). This implementation is partial and not yet complete, the available features are:

- Client (Publish, Subscribe)
- Broker (Central Server)
- Singular topics
- QoS 0
- QoS 1 *(Experimental)*

## Building and Testing
To build the project (for development purposes) follow these steps:
1. `git clone` this repository
2. use `cabal build` in the root directory `mqtt-haskell`
3. use `cabal run mqtt-broker` to spawn a broker and wait for it to finish linking
4. then use `cabal run mqtt-client` to start a test client 

The project can be tested with the following commands:

```shell
stack test
cabal test
```

Or in watch mode:
```shell
stack test --file-watch
```
## Examples
The client can be configured via the `MqttConfig` datatype. Below the description of this datatype and examples for an implementation of the client and the broker can be found.

MqttConfig:
```Haskell
type CID = String 
type Host = String
type Token = String

data MqttConfig = MqttConfig {
    cid::CID
  , host::Host
  , port::PortNumber
  , token::Maybe Token
}
```

Broker:
```Haskell
module Main where
import Broker (createBroker)

main :: IO ()
main = do
    createBroker 8000 (Just "supersecretpassword")
    _ <- getLine
    putStrLn "Quit"
```

Client:
```Haskell
module Main where

import Client
import Client.Subscription
import Client.Connection
import Client.MqttConfig

main :: IO ()
main = do
  clientId <- getLine
  conn <- open (MqttConfig clientId "127.0.0.1" 8000 (Just "supersecretpassword")) subscriptions
  chat conn

chat :: Connection -> IO ()
chat conn = do
    msg <- getLine
    (send `apply` conn) ("topic1", msg)
    chat conn

subscriptions :: Subscription
subscriptions = subGroup [topic1Sub]

topic1Sub :: Subscription
topic1Sub = sub "topic1" (pure customDataHandler)

customDataHandler :: String -> IO ()
customDataHandler d = putStrLn $ "Received: " ++ d
```