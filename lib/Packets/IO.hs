module Packets.IO where

import Packets.Abstract ( PacketId )
import Data.IORef ( modifyIORef, newIORef, readIORef )

type PacketIdCounter = (IO PacketId, IO PacketId)

-- https://stackoverflow.com/a/16811455/11047164
mkPacketIdCounter :: IO PacketIdCounter
mkPacketIdCounter = do
  r <- newIORef 0
  let peek = readIORef r
      incr = (do
        modifyIORef r (+1)
        peek)
  return (peek, incr)