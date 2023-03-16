module Main where
import Packets (QoS(Zero))
import Client

main :: IO ()
main = do
    client <- clientOpen "Client1" (ClientConfig{hostname="127.0.0.1", portnumber=8000, quality=Zero}) subscriptions
    _ <- getLine
    clientClose client

subscriptions :: Subscription
subscriptions = subGroup [topic1Sub, topic2Sub]

topic1Sub :: Subscription
topic1Sub = sub "Topic1" $ \client msg -> do
    putStrLn msg
    clientSend client "Topic2" "Hello world!"

topic2Sub :: Subscription
topic2Sub = sub "Topic2" $ \client msg -> do
    putStrLn msg