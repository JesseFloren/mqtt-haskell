module Client where
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (unpack)

runClient :: IO ()
runClient = do
    -- create socket
    sock <- subscribe "127.0.0.1" 8000
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

