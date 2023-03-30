-- module Chat.Terminal where

-- import System.Console.ANSI (cursorUp, clearLine, clearScreen, setCursorPosition)

-- prompt :: String -> IO String
-- prompt q = do
--   putStrLn q
--   a <- getLine
--   cursorUp 1
--   clearLine
--   cursorUp 1
--   clearLine
--   return a

-- resetScreen :: IO ()
-- resetScreen = do
--   clearScreen
--   setCursorPosition 0 0