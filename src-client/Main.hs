module Main where

import Client
import Socket

main :: IO ()
main = do
    clientId <- getLine
    conn <- runClient (MqttConfig clientId "127.0.0.1" 8000 Nothing) subscriptions
    chat conn

chat :: Connection -> IO ()
chat conn = do
    msg <- getLine
    (send `apply` conn) ("topic1", msg)
    chat conn

subscriptions :: Subscription
subscriptions = subGroup [topic1Sub]

topic1Sub :: Subscription
topic1Sub = sub "topic1" (pure putStrLn)