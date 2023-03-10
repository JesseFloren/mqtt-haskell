module Client where
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (unpack)

type Connection = Socket

newtype IPAddress = IP String
newtype Port = Port Int
data SocketAddress = SocketAddress IPAddress Port

runClient :: IO ()
runClient = do
    -- create socket
    let ip = IP "127.0.0.1"
        port = Port 8000
    sock <- subscribe (SocketAddress ip port)
    -- get input, send message
    s <- Prelude.getLine
    sendAll sock $ C.pack s
    -- receive response
    response <- recv sock 1024
    putStrLn $ "Received: " ++ show response
    -- create type of client based on input
    createClient sock s

    where 
        createClient sock "pub" = do
            putStrLn "Entering pub mode" 
            talkToServer sock 
        createClient sock _     = do
            putStrLn "Entering sub mode" 
            listenToServer sock

{-| 
    Takes a terminal input and sends it of to the server. The gathering of the terminal input is
    a blocking operation
-}
talkToServer :: Socket -> IO ()
talkToServer sock = do
    s <- Prelude.getLine
    sendAll sock $ C.pack s
    talkToServer sock

-- | Waits for a message from the server and then prints it on screen. The recv function is blocking
listenToServer :: Socket -> IO ()
listenToServer sock = do
    response <- recv sock 1024
    putStrLn $ "Received: " ++ unpack response
    listenToServer sock

subscribe :: SocketAddress -> IO Socket
subscribe socketAddress = do
  -- Create socket
  sock <- socket AF_INET Stream defaultProtocol
  serverAddr <- createSocketAddress socketAddress
  
  -- Connect socket to address
  connect sock serverAddr

  return sock
    where 
      createSocketAddress :: SocketAddress -> IO SockAddr
      createSocketAddress (SocketAddress (IP ip) (Port port)) = do
        info <- getAddrInfo 
                  (Just defaultHints { addrFlags = [AI_ADDRCONFIG] }) 
                  (Just ip) 
                  (Just (show port)) 
        return (head $ map addrAddress info)

