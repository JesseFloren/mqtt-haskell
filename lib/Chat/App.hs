module Chat.App (run) where

import Chat.Message (Message(..))
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (cursorUp)

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

import qualified Control.Concurrent.Async as A
import Client (IPAddress(..), Port(..), SocketAddress(..), subscribe)

type Chat = [Message]

data AppState = AppState {username :: String, chat :: Chat, socket :: Socket}

putMessage :: AppState -> Message -> AppState
putMessage (AppState user chat' sock) msg = 
  AppState user (msg : chat') sock

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
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let promptMessage = Message <$> getLine <*> return (username state)
          -- wait for either the user or the server to send a message
          awaitChatEvent = promptMessage `A.race` receiveMessage (socket state)
      
      newState <- handleChatEvent state =<< awaitChatEvent
      let newChat = chat newState

      putStr (showChat newChat)

      cursorUp (length newChat)
      putStrLn ""
      runLoop newState

showChat :: Chat -> String
showChat = unlines . map show . reverse 

receiveMessage :: Socket -> IO Message 
receiveMessage sock = do
  response <- recv sock 1024
  return (Message (C.unpack response) "<unknown user>")

handleChatEvent :: AppState -> ChatEvent -> IO AppState
handleChatEvent state (Left userMsg) = do
  sendMessage (socket state) userMsg
  return state
handleChatEvent state (Right serverMsg) = return (putMessage state serverMsg)



-- TODO move to own file
sendMessage :: Socket -> Message -> IO ()
sendMessage sock msg = do
  sendAll sock $ C.pack $ show msg

login :: IO AppState
login = AppState <$> promptUsername <*> getMessages <*> ioSocket
  where 
    ip = IP "127.0.0.1"
    port = Port 8000
    ioSocket = subscribe (SocketAddress ip port)

promptUsername :: IO String
promptUsername = do
  prompt "What is your username?"
  -- login should also then connect 

getMessages :: IO [Message]
getMessages = pure []

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state ++ " (connected to " ++ show (socket state) ++ ")"
