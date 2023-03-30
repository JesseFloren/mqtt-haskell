module Main where
import Broker (createBroker)


--- *** Broker *** ---
main :: IO ()
main = do
    createBroker 8000
    _ <- getLine
    putStrLn "Quit"
