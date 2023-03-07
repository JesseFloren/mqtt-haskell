module Chat.Message (Message(..)) where

data Message = Message { message :: String, author :: String }

instance Show Message where
<<<<<<< HEAD
  show (Message msg auth) = auth ++ ": " ++ msg
=======
  show (Message message author) = author ++ ": " ++ message
>>>>>>> f049921 (very basic implementation of the loop of a chat app)
