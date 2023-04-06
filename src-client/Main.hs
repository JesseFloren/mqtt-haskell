module Main where

import Client
import Client.Subscription
import Client.Connection
import Client.MqttConfig
import Control.Exception.Base
import Utils (MqttException)

main :: IO ()
main = do
  clientId <- getLine
  conn <- open (MqttConfig clientId "127.0.0.1" 8000 (Just "supersecretpassword")) subscriptions
  res <- try (chat conn) :: IO (Either MqttException ())
  case res of
    Left _ -> putStrLn "Disconnect from server"
    Right _ -> putStrLn "Gracefull close"

chat :: Connection -> IO ()
chat conn = do
    msg <- getLine
    case msg of 
      "close" -> close' conn
      _       -> do
        -- Sending can throw an exception
        (send `apply` conn) ("topic1", msg)
        chat conn
    

subscriptions :: Subscription
subscriptions = subGroup [topic1Sub]

topic1Sub :: Subscription
topic1Sub = sub "topic1" (pure customDataHandler)

customDataHandler :: String -> IO ()
customDataHandler d = putStrLn $ "Received: " ++ d