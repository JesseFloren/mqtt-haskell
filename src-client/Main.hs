module Main where

import qualified Client as Client
import qualified Socket as Sock
import qualified Chat.App as App
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    App.run
    -- runClient


runClient :: IO ()
runClient = do
    -- create socket
    let ip = "127.0.0.1"
        port = 8000
        address = Sock.socketAddress ip port
    sock <- Client.open address

    -- get input, send message
    s <- Prelude.getLine
    Client.send s sock

    -- receive response
    loop sock
    
    -- -- create type of client based on input
    -- createClient sock s
    Client.close sock

    where 
      loop :: Client.Connection -> IO ()
      loop conn = do
        response <- Client.receive conn `A.race` (getLine >>= \s -> Client.send s conn) `A.race` threadDelay 100 
        putStrLn $ "Received: " ++ show response
        loop conn
      

      -- createClient sock "pub" = do
      --     putStrLn "Entering pub mode" 
      --     talkToServer sock 
      -- createClient sock _     = do
      --     putStrLn "Entering sub mode" 
      --     listenToServer sock
