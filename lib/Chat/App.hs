module Chat.App (run) where

import Chat.Message (Message(..))
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (cursorUp, clearLine, clearScreen, setCursorPosition)

import qualified Control.Concurrent.Async as A
import qualified Client as Client

type Chat = [Message]

data AppState = AppState {username :: String, chat :: Chat, socket :: Client.Connection}

putMessage :: AppState -> Message -> AppState
putMessage (AppState user chat' sock) msg = 
  AppState user (msg : chat') sock

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state ++ " (connected to " ++ show (socket state) ++ ")"

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
  
  Client.close (socket state)
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let promptMessage = Message <$> getLine <*> return (username state)
          -- wait for either the user or the server to send a message
          awaitChatEvent = promptMessage `A.race` receiveMessage (socket state)
      
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

receiveMessage :: Client.Connection -> IO Message 
receiveMessage sock = do
  response <- Client.receive sock
  return (Message response "<unknown user>")

handleChatEvent :: AppState -> ChatEvent -> IO AppState
handleChatEvent state (Left userMsg) = do
  sendMessage (socket state) userMsg
  return state
handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)


-- TODO move to own file
sendMessage :: Client.Connection -> Message -> IO ()
sendMessage sock msg = Client.send (show msg) sock

login :: IO AppState
login = AppState <$> promptUsername <*> getMessages <*> ioSocket
  where 
    ip = "127.0.0.1"
    port = 8000
    ioSocket = Client.open (Client.socketAddress ip port)

promptUsername :: IO String
promptUsername = do
  prompt "What is your username?"
  -- login should also then connect 

getMessages :: IO [Message]
getMessages = pure []

