-- module Chat.App (run) where

-- import Chat.Message (Message(..))
-- import Chat.Terminal (prompt, resetScreen)
-- import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
-- import System.Console.ANSI (setCursorPosition)

<<<<<<< HEAD
-- import qualified Control.Concurrent.Async as A
-- import qualified Client as Client
-- import qualified Socket as Sock
=======
import qualified Control.Concurrent.Async as A
import qualified Client
import Client.Connection 
import qualified Socket as Sock
>>>>>>> origin/23-setup-packet-sending-for-client

-- type Chat = [Message]

<<<<<<< HEAD
-- data AppState = AppState {username :: String, chat :: Chat, socket :: Client.Connection}
=======
data AppState = AppState {username :: String, chat :: Chat, conn :: Client.Connection}
>>>>>>> origin/23-setup-packet-sending-for-client

-- putMessage :: AppState -> Message -> AppState
-- putMessage (AppState user chat' sock) msg = 
--   AppState user (msg : chat') sock

<<<<<<< HEAD
-- printStateInfo :: AppState -> IO ()
-- printStateInfo state = putStrLn $ "Logged in as " ++ username state ++ " (connected to " ++ show (socket state) ++ ")"
=======
printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state ++ " (connected to " ++ show (conn state) ++ ")"
>>>>>>> origin/23-setup-packet-sending-for-client

-- -- Left user message, Right server message
-- -- TODO refine this into its own data type
-- type ChatEvent = Either Message Message

-- run :: IO ()
-- run = do
--   hSetBuffering stdout LineBuffering
--   resetScreen
  
--   state <- login
  
--   printStateInfo state
--   runLoop state
  
<<<<<<< HEAD
--   Client.close (socket state)
--   where 
--     runLoop :: AppState -> IO ()
--     runLoop state = do
--       let promptMessage = Message <$> getLine <*> return (username state)
--           -- wait for either the user or the server to send a message
--           awaitChatEvent = promptMessage `A.race` receiveMessage (socket state)
=======
  Client.close `apply` conn state
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let promptMessage = Message <$> getLine <*> return (username state)
          -- wait for either the user or the server to send a message
          awaitChatEvent = promptMessage `A.race` (receiveMessage `apply` conn state)
>>>>>>> origin/23-setup-packet-sending-for-client
      
--       newState <- handleChatEvent state =<< awaitChatEvent
--       let newChat = chat newState

--       refreshChat newChat
--       runLoop newState

-- refreshChat :: Chat -> IO ()
-- refreshChat cs = do
--   setCursorPosition 1 0
--   putStr (showChat cs)

-- showChat :: Chat -> String
-- showChat = unlines . map show . reverse 

<<<<<<< HEAD
-- receiveMessage :: Client.Connection -> IO Message 
-- receiveMessage sock = do
--   response <- Client.receive sock
--   return (Message response "<unknown user>")

-- handleChatEvent :: AppState -> ChatEvent -> IO AppState
-- handleChatEvent state (Left userMsg) = do
--   sendMessage (socket state) userMsg
--   return state
-- handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)


-- -- TODO move to own file
-- sendMessage :: Client.Connection -> Message -> IO ()
-- sendMessage sock msg = Client.send (show msg) sock
=======
receiveMessage :: ConnAction (IO Message)
receiveMessage = do
  receive <- Client.receive
  return ((`Message` "<unknown user>") <$> receive)

handleChatEvent :: AppState -> ChatEvent -> IO AppState
handleChatEvent state (Left userMsg) = do
  (sendMessage `apply` conn state) userMsg
  return state
handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)


-- TODO move to own file
sendMessage :: ConnAction (Message -> IO ())
sendMessage = do
  send <- Client.send
  return (send . show)
>>>>>>> origin/23-setup-packet-sending-for-client

-- login :: IO AppState
-- login = AppState <$> promptUsername <*> getMessages <*> ioSocket
--   where 
--     ip = "127.0.0.1"
--     port = 8000
--     ioSocket = Client.open (Sock.socketAddress ip port)

-- promptUsername :: IO String
-- promptUsername = do
--   prompt "What is your username?"
--   -- login should also then connect 

-- getMessages :: IO [Message]
-- getMessages = pure []

