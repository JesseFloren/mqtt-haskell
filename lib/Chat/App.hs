module Chat.App (run) where

import Chat.Message (Message(..))
<<<<<<< HEAD
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (cursorUp)
=======
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (cursorUp, clearLine, clearScreen, setCursorPosition)
>>>>>>> f049921 (very basic implementation of the loop of a chat app)

type Chat = [Message]

data AppState = AppState {username :: String, chat :: Chat}

<<<<<<< HEAD
putMessage :: AppState -> String -> AppState
putMessage (AppState user chat') msg = 
  AppState user (Message {message = msg, author = user} : chat')

=======
emptyState :: String -> AppState
emptyState u = AppState u []

prompt :: String -> IO String
prompt q = do
  putStrLn q
  a <- getLine
  cursorUp 1
  clearLine
  cursorUp 1
  clearLine
  return a

resetScreen :: IO ()
resetScreen = do
  clearScreen
  setCursorPosition 0 0

login :: IO String
login = prompt "What is your username?"

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state
>>>>>>> f049921 (very basic implementation of the loop of a chat app)

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  resetScreen
<<<<<<< HEAD
  state <- AppState <$> login <*> getMessages
=======
  state <- emptyState <$> login
>>>>>>> f049921 (very basic implementation of the loop of a chat app)
  printStateInfo state
  runLoop state
  where 
    runLoop :: AppState -> IO ()
    runLoop state = do
      let chat' = chat state
      putStr (showChat chat')
      msg <- getLine
      cursorUp (2 + length chat')
      putStrLn ""
<<<<<<< HEAD
      runLoop (putMessage state msg)

    getMessages :: IO [Message]
    getMessages = pure []

login :: IO String
login = do
  prompt "What is your username?"
  -- login should also then connect 

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state

showChat :: Chat -> String
showChat = unlines . map show . reverse 
=======
      runLoop (sendMessage state msg)


sendMessage :: AppState -> String -> AppState
sendMessage (AppState user chat) msg = 
  AppState user (Message {message = msg, author = user} : chat)

showChat :: Chat -> String
showChat = unlines . map show . reverse 
>>>>>>> f049921 (very basic implementation of the loop of a chat app)
