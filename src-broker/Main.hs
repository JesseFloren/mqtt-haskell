module Main where
import Broker (createBroker)

main :: IO ()
main = do
    createBroker 8000 (Just "supersecretpassword")
    _ <- getLine
    putStrLn "Quit"
