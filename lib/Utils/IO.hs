module Utils.IO where

import Packets.Abstract ( PacketId )
import Data.IORef ( modifyIORef, newIORef, readIORef )

type PacketIdCounter = (IO PacketId, IO PacketId)

mkPacketIdCounter :: IO PacketIdCounter
mkPacketIdCounter = do
  r <- newIORef 0
  let peek = readIORef r
      incr = (do
        modifyIORef r (+1)
        peek)
  return (peek, incr)

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

whenJust2 :: (Maybe a, Maybe b) -> ((a, b) -> IO ()) -> IO ()
whenJust2 (Just x, Just y) f = f (x, y)
whenJust2 _ _ = return ()