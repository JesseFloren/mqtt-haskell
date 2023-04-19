module Main where
import Broker (createBroker, BrokerConfig(..))

main :: IO ()
main = do
    createBroker (BrokerConfig 8000 (Just "supersecretpassword") 1000)
    putStrLn "Started Broker on port 8000, press any key to close the broker"
    _ <- getLine
    putStrLn "Quit"
