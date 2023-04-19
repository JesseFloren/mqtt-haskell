module Main where
import Broker (createBroker, BrokerConfig(..))

main :: IO ()
main = do
    createBroker (BrokerConfig 8000 (Just "supersecretpassword") 1000)
    _ <- getLine
    putStrLn "Quit"
