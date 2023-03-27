{-# LANGUAGE LambdaCase #-}
module Client where
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (unpack)
import Socket.Base (createSocket, sendPacket, recvPacket)
import Packets.Simple (writeConnectPacket, readConnackPacket, writeSubscribePacket, readSubackPacket, writePublishPacket, readPublishPacket)
import Packets.Abstract (ConnectFlags(ConnectFlags), ConnackResponse (..), QoS (Zero), PublishFlags (PublishFlags, channel), Packet (cmd), CommandType (..))
import Control.Concurrent (forkIO)

runClient :: IO ()
runClient = do
    sock <- createSocket ("127.0.0.1", 8000)
    handleConnect sock
    handleSubscribe sock
    putStrLn "Connected with broker successfully"
    _ <- forkIO $ talkToServer sock
    listenToServer sock
    close sock
 
handleConnect :: Socket -> IO ()
handleConnect sock = do
    sendPacket sock $ writeConnectPacket "Client1" (ConnectFlags Nothing Nothing Nothing False) 60000
    connack <- recvPacket sock >>= (\case {Just x -> return $ readConnackPacket x; Nothing -> return Nothing})
    case connack of
        Just (_, Accepted) -> return ()
        _ -> error "Failed to connect"

handleSubscribe :: Socket -> IO ()
handleSubscribe sock = do
    sendPacket sock $ writeSubscribePacket 0 [("topic1", Zero)]
    suback <- recvPacket sock >>= (\case {Just x -> return $ readSubackPacket x; Nothing -> return Nothing})
    case suback of
        Just (_, _) -> return ()
        _ -> error "Failed to subscribe"

talkToServer :: Socket -> IO ()
talkToServer sock = do
    s <- Prelude.getLine
    sendPacket sock $ writePublishPacket 1 (PublishFlags False False ("topic1", Zero)) s
    talkToServer sock

listenToServer :: Socket -> IO ()
listenToServer sock = do
    response <- recvPacket sock
    case response of
        Nothing -> return ()
        Just packet -> do 
            case cmd packet of
                PUBLISH -> do
                    handlePublish packet
                    listenToServer sock
                _ -> listenToServer sock
            
handlePublish :: Packet -> IO ()
handlePublish packet = do
    case readPublishPacket packet of
        Nothing -> return ()
        Just (_, flags, message) -> do
            putStrLn $ "Received " ++ fst (channel flags) ++ ": " ++ message
