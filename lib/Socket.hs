-- | Contains socket utilities
module Socket (createSockAddr, socketAddress, SocketAddress(..), Port(..)) where

import Network.Socket (addrFlags, getAddrInfo, defaultHints, addrAddress)
import qualified Network.Socket as S

newtype HostAddress = HostAddress String
newtype Port = Port Int

data SocketAddress = SocketAddress HostAddress Port


socketAddress :: String -> Int -> SocketAddress
socketAddress ip port = SocketAddress (HostAddress ip) (Port port)

createSockAddr :: SocketAddress -> IO S.SockAddr
createSockAddr (SocketAddress (HostAddress ip) (Port port)) = do
  info <- getAddrInfo 
            (Just defaultHints { addrFlags = [S.AI_ADDRCONFIG] }) 
            (Just ip) 
            (Just (show port)) 
  return (head $ map addrAddress info)
