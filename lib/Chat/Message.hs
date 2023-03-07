module Chat.Message (Message(..)) where

data Message = Message { message :: String, author :: String }

instance Show Message where
  show (Message message author) = author ++ ": " ++ message
