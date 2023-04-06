{-# LANGUAGE InstanceSigs #-}
module Client.Connection (Connection(..), ConnAction(..), apply, getSock, getNextPacketId, getConn, returnIO, chainM) where

import qualified Network.Socket as S
import Packets (PacketIdCounter)
import Packets.Abstract
import Control.Monad ( (>=>) )
import Control.Concurrent

data Connection = Conn {
    sock :: S.Socket
  , nextPacketId :: PacketIdCounter
  , threadId :: ThreadId
} 

instance Show Connection where
  show = show . sock

newtype ConnAction a = CA (Connection -> a)

instance Functor ConnAction where
  fmap :: (a -> b) -> ConnAction a -> ConnAction b
  fmap f (CA a) = CA (f . a)

instance Applicative ConnAction where
  pure :: a -> ConnAction a 
  pure = CA . const

  (<*>) :: ConnAction (a -> b) -> ConnAction a -> ConnAction b
  (<*>) (CA f) (CA a) = CA (\conn -> f conn (a conn))

instance Monad ConnAction where
  (>>=) :: ConnAction a -> (a -> ConnAction b) -> ConnAction b
  (>>=) (CA a) f = CA (\conn -> f (a conn) `apply` conn)

apply :: ConnAction a -> Connection -> a
apply (CA f) = f

getSock :: ConnAction S.Socket
getSock = CA (\(Conn sock _ _) -> sock)

getNextPacketId :: ConnAction (IO PacketId)
getNextPacketId = CA (\(Conn _ (_, nextPacketId) _) -> nextPacketId)

getConn :: ConnAction Connection
getConn = CA id

returnIO :: a -> ConnAction (IO a)
returnIO ioA = return $ do return ioA

-- TODO find better name
chainM :: Monad m => (a -> m b) -> ConnAction (b -> m c) -> ConnAction (a -> m c)
chainM f1 f2 = do
  conn <- getConn
  return (f1 >=> (f2 `apply` conn))
