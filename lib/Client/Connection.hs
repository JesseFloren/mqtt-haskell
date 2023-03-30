{-# LANGUAGE InstanceSigs #-}
module Client.Connection (Connection(..), ConnAction(..), apply, getSock, getNextPacketId, getConn, returnIO, chainM) where

import qualified Network.Socket as S
import Packets.IO (PacketIdCounter)
import Packets.Abstract

data Connection = Conn {
    sock :: S.Socket
  , nextPacketId :: PacketIdCounter
} 

instance Show Connection where


newtype ConnAction a = CA (Connection -> a)

apply :: ConnAction a -> Connection -> a
apply (CA f) = f

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

getSock :: ConnAction S.Socket
getSock = CA (\(Conn sock _) -> sock)

getNextPacketId :: ConnAction (IO PacketId)
getNextPacketId = CA (\(Conn _ (_, nextPacketId)) -> nextPacketId)

getConn :: ConnAction Connection
getConn = CA id

returnIO :: a -> ConnAction (IO a)
returnIO ioA = return $ do return ioA

-- TODO find better name
chainM :: Monad m => (a -> m b) -> ConnAction (b -> m c) -> ConnAction (a -> m c)
chainM f1 f2 = do
  conn <- getConn
  return $ \a -> f1 a >>= (f2 `apply` conn) 
