module Chat.App (run) where

import Chat.Message (Message(..))
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (setCursorPosition)

import qualified Control.Concurrent.Async as A
import qualified Client
import Packets (QoS (..))

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
  
  Client.close `Client.apply` conn state
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let promptMessage = Message <$> getLine <*> return (username state)
          -- wait for either the user or the server to send a message
          awaitChatEvent = promptMessage `A.race` (receiveMessage `Client.apply` conn state)
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

receiveMessage :: Client.ConnAction (IO Message)
receiveMessage = do
  receive <- Client.receive
  return ((`Message` "<unknown user>") <$> receive)

handleChatEvent :: AppState -> ChatEvent -> IO AppState
handleChatEvent state (Left userMsg) = do
  (sendMessage `Client.apply` conn state) userMsg
  return state
handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)


-- TODO move to own file
-- TODO implement topics
sendMessage :: Client.ConnAction (Message -> IO ())
sendMessage = do
  send <- Client.send Zero
  return (\msg -> send ("", show msg))

login :: IO AppState
login = do
  un <- promptUsername
  let ip = "127.0.0.1"
      port = 8000
      ioSocket = Client.open (Client.MqttConfig un ip port 1000 (Just "supersecretpassword")) Client.empty
  AppState <$> pure un <*> getMessages <*> ioSocket
      

promptUsername :: IO String
promptUsername = do
  prompt "What is your username?"

getMessages :: IO [Message]
getMessages = pure []

