module Main where
import Client (runClient)
import Packets

main :: IO ()
main = do
    runClient
