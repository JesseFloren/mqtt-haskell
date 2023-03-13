module Main where
import Client (runClient)
import Packets

main :: IO ()
main = do
    print $ readPublishPacket $ writePublishPacket 10 (PublishFlags{dup=True, retain=True, channel=("topic", Two)}) "Hello"
    