module Chat.App (run) where

import Chat.Message (Message(..))
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (setCursorPosition)

import qualified Control.Concurrent.Async as A
import qualified Client as Client
import Client.Connection 
import qualified Socket as Sock

type Chat = [Message]

data AppState = AppState {username :: String, chat :: Chat, conn :: Client.Connection}

putMessage :: AppState -> Message -> AppState
putMessage (AppState user chat' sock) msg = 
  AppState user (msg : chat') sock

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state ++ " (connected to " ++ show (conn state) ++ ")"

-- Left user message, Right server message
-- TODO refine this into its own data type
type ChatEvent = Either Message Message

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  resetScreen
  
  state <- login
  
  printStateInfo state
  runLoop state
  
  Client.close `apply` (conn state)
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let promptMessage = Message <$> getLine <*> return (username state)
          -- wait for either the user or the server to send a message
          awaitChatEvent = promptMessage `A.race` (receiveMessage `apply` (conn state))
      
      newState <- handleChatEvent state =<< awaitChatEvent
      let newChat = chat newState

      refreshChat newChat
      runLoop newState

refreshChat :: Chat -> IO ()
refreshChat cs = do
  setCursorPosition 1 0
  putStr (showChat cs)

showChat :: Chat -> String
showChat = unlines . map show . reverse 

receiveMessage :: ConnAction (IO Message)
receiveMessage = do
  receive <- Client.receive
  return ((\resp -> Message resp "<unknown user>") <$> receive)

handleChatEvent :: AppState -> ChatEvent -> IO AppState
handleChatEvent state (Left userMsg) = do
  (sendMessage `apply` conn state) userMsg
  return state
handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)


-- TODO move to own file
sendMessage :: ConnAction (Message -> IO ())
sendMessage = do
  send <- Client.send
  return (\msg -> send (show msg))

login :: IO AppState
login = AppState <$> promptUsername <*> getMessages <*> ioSocket
  where 
    ip = "127.0.0.1"
    port = 8000
    ioSocket = Client.open (Sock.socketAddress ip port)

promptUsername :: IO String
promptUsername = do
  prompt "What is your username?"
  -- login should also then connect 

getMessages :: IO [Message]
getMessages = pure []

