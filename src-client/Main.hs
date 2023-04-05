module Main where

import Client
import Client.Subscription
import Client.Connection
import Client.MqttConfig
import Control.Exception.Base

main :: IO ()
main = do
  clientId <- getLine
  conn <- open (MqttConfig clientId "127.0.0.1" 8000 (Just "supersecretpassword")) subscriptions
  chat conn

chat :: Connection -> IO ()
chat conn = do
    msg <- getLine
    result <- try ((send `apply` conn) ("topic1", msg)) :: IO (Either SomeException ())
    case result of
      Left _  -> putStrLn "Connection Interrupted"
      Right _ -> chat conn
    

subscriptions :: Subscription
subscriptions = subGroup [topic1Sub]

topic1Sub :: Subscription
topic1Sub = sub "topic1" (pure customDataHandler)

customDataHandler :: String -> IO ()
customDataHandler d = putStrLn $ "Received: " ++ d