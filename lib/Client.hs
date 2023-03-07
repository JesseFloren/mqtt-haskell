module Client where
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

runClient :: IO ()
runClient = do
    -- create socket
    sock <- subscribe "127.0.0.1" 8000
    
    -- send message
    sendAll sock $ C.pack "Hello, server!\n"
    
    -- receive response
    response <- recv sock 1024
    putStrLn $ "Received: " ++ show response
    
    -- close connection
    -- close sock
    talkToServer sock

talkToServer :: Socket -> IO ()
talkToServer sock = do
    s <- Prelude.getLine
    -- putStrLn s
    sendAll sock $ C.pack s
    
    -- receive response
    response <- recv sock 1024
    putStrLn $ "Received: " ++ show response
    talkToServer sock

subscribe :: String -> Int -> IO Socket
subscribe addr port = do
    -- Create socket
    sock <- socket AF_INET Stream defaultProtocol

    -- Create address
    serverAddr <- getAddrInfo (Just defaultHints { addrFlags = [AI_ADDRCONFIG] }) (Just addr) (Just (show port)) 
        >>= \x -> return (head $ map addrAddress x)
    
    -- Connect socket to address
    connect sock serverAddr

    return sock

