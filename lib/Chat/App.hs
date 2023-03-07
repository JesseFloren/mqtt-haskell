module Chat.App (run) where

import Chat.Message (Message(..))
import Chat.Terminal (prompt, resetScreen)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Console.ANSI (cursorUp, clearLine, clearScreen, setCursorPosition)

type Chat = [Message]

data AppState = AppState {username :: String, chat :: Chat}

putMessage :: AppState -> String -> AppState
putMessage (AppState user chat') msg = 
  AppState user (Message {message = msg, author = user} : chat')

emptyState :: String -> AppState
emptyState u = AppState u []

putMessage :: AppState -> String -> AppState
putMessage (AppState user chat) msg = 
  AppState user (Message {message = msg, author = user} : chat)

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  resetScreen
  state <- AppState <$> login <*> getMessages
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
      runLoop (sendMessage state msg)

login :: IO String
login = prompt "What is your username?"

printStateInfo :: AppState -> IO ()
printStateInfo state = putStrLn $ "Logged in as " ++ username state

showChat :: Chat -> String
showChat = unlines . map show . reverse 
