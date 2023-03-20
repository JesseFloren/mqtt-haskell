module Main where
import Client (runClient)
import qualified Chat.App as App

main :: IO ()
main = do
    App.run
    --runClient
