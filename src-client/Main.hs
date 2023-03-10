module Main where
import Client (runClient)
import Packets

main :: IO ()
main = do
    print $ byteStringToPacket $ packetToByteString (
        connectMessage "Client12" (ConnectFlags{username=(Just "Username"), password=(Just "Password"), will=(Just (False, One, "topic", "message")), cleanSession=True}) 6000)