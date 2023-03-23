module Main where

import Client.Connection
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
    conn <- Client.open address

    -- get input, send message
    s <- Prelude.getLine
    (Client.send `apply` conn) s

    -- receive response
    loop `apply` conn
    
    -- -- create type of client based on input
    -- createClient sock s
    Client.close `apply` conn

    where 
      loop :: ConnAction (IO ())
      loop = do
        send <- Client.send
        receive <- Client.receive
        let loop' :: IO ()
            loop' = do
              response <- receive `A.race` (getLine >>= \s -> send s) `A.race` threadDelay 100 
              putStrLn $ "Received: " ++ show response
              loop'
        return loop'
      

      -- createClient sock "pub" = do
      --     putStrLn "Entering pub mode" 
      --     talkToServer sock 
      -- createClient sock _     = do
      --     putStrLn "Entering sub mode" 
      --     listenToServer sock
