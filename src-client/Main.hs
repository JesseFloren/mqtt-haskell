module Main where

import Client
import Control.Exception.Base
import Packets (QoS(..))

main :: IO ()
main = do
  putStrLn "To connect to the broker, provide clientId:"
  clientId <- getLine
  conn <- open (MqttConfig clientId "127.0.0.1" 8000 1000 (Just "supersecretpassword")) subscriptions
  res <- try (chat conn) :: IO (Either MqttException ())
  case res of
    Left _ -> putStrLn "Disconnect from server"
    Right _ -> putStrLn "Gracefull close"

chat :: Connection -> IO ()
chat conn = do
    msg <- getLine
    case msg of 
      "close" -> close_ conn
      _       -> do
        -- Sending can throw an exception
        (send Zero `apply` conn) ("topic1", msg ++ " (QoS0)")
        (send One `apply` conn) ("topic2", msg ++ " (QoS1)")
        chat conn
    
subscriptions :: Subscription
subscriptions = subGroup [topic1Sub, topic2Sub]

topic1Sub :: Subscription
topic1Sub = sub ("topic1", Zero) (pure customDataHandler)

topic2Sub :: Subscription
topic2Sub = sub ("topic2", One) (pure customDataHandler)

customDataHandler :: String -> IO ()
customDataHandler d = putStrLn $ "Received: " ++ d