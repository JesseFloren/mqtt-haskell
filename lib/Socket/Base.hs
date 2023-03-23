module Socket.Base where
import Network.Socket
import Packets.Abstract ( Packet )
import Packets.Builder ( packetToByteString )
import Packets.Parser ( byteStringToPacket )
import Network.Socket.ByteString ( recv, sendAll )

--- *** Server Side *** ---
createServer :: PortNumber -> IO Socket
createServer port = do
    sock <- socket AF_INET Stream defaultProtocol
    serverAddr <- formatAddress (Just "127.0.0.1", Just port)
    bind sock serverAddr
    listen sock 5
    return sock

--- *** Client Side *** ---
createSocket :: (HostName, PortNumber) -> IO Socket
createSocket (host, port) = do
    sock <- socket AF_INET Stream defaultProtocol
    serverAddr <- formatAddress (Just host, Just port)
    connect sock serverAddr
    return sock

formatAddress :: (Maybe HostName, Maybe PortNumber) -> IO SockAddr
formatAddress (host, port) = do
    addr <- getAddrInfo (Just defaultHints { addrFlags = [AI_ADDRCONFIG] }) host (show <$> port)
    return (addrAddress $ head addr)

--- *** Send & Recv *** ---
sendPacket :: Socket -> Packet -> IO ()
sendPacket sock = sendAll sock . packetToByteString

recvPacket :: Socket -> IO Packet
recvPacket sock = do
    response <- byteStringToPacket <$> recv sock 1024 
    case response of
        (Just x) -> return x
        Nothing -> do
            putStrLn "Failed to decode packet"
            recvPacket sock
