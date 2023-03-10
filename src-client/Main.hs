module Main where
import Client (runClient)
import Utils (helloWorld)
import Header (bitsToInt, Bit (..))
import qualified Chat.App as App

main :: IO ()
main = do
    App.run
<<<<<<< HEAD
    --runClient
=======
    -- runClient
>>>>>>> 424c3a9 (integrate terminal chat app with client code)
