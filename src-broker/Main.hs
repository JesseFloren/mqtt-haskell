module Main where
import Broker (createBroker)

main :: IO ()
main = do
    createBroker 8000
    _ <- getLine
    putStrLn "Quit"
