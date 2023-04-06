{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Broker where

import Network.Socket ( Socket, PortNumber, accept, close)
import Control.Concurrent
import Control.Exception.Base ( SomeException, try )
import Socket.Base (createServer, recvPacket, sendPacket)
import Utils.Queue ( Queue (..), pop, push, single )
import Control.Applicative ( Alternative((<|>)) )
import Packets
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import qualified Data.Map as M
-- import Debug.Trace (trace, traceShow)
import Data.Either ( isLeft )

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId, sThread :: ThreadId }

-- TODO move to Broker.Session
data Session = Session {
  clientId :: String,
  subscriptions :: M.Map Topic QoS,
  keepAlive :: Int,
  will :: Maybe (Retain, QoS, Topic, String),
  pending :: [Message],
  conn :: Maybe Socket,
  listenThread :: ThreadId
} deriving (Show)

data Message = Message {topic :: Topic, msg :: String, pid :: PacketId} deriving (Show)

type Token = Maybe String

createBroker :: PortNumber -> Token -> Int -> IO MqttBroker
createBroker port sSecret resendDelay = do
    sock <- createServer port
    sessions <- newMVar []
    messages <- newEmptyMVar
    sThread <- forkIO $ sessionHandler sessions resendDelay
    aThread <- forkIO $ acceptClientLoop sock sSecret messages sessions
    mThread <- forkIO $ messageQueue messages sessions
    return $ MqttBroker sock aThread mThread sThread

messageQueue :: MVar (Queue Message) -> MVar [Session] -> IO ()
messageQueue messages sessions = do
    (message, queue) <- pop <$> takeMVar messages
    case message of
        Nothing -> messageQueue messages sessions
        Just msg -> do
            modifyMVar_ sessions (mapM (sendMessage msg False))
            case queue of
                End -> messageQueue messages sessions
                _ -> do
                    modifyMVar_ messages (\_ -> return queue)
                    messageQueue messages sessions

sendMessage :: Message -> Dup -> Session -> IO Session
sendMessage m@(Message topic msg pid) dup s@(Session {..}) = case (conn, M.member topic subscriptions) of
    (Just sock, True) -> do
      let qos = subscriptions M.! topic
          pkt = writePublishPacket pid (PublishFlags dup False (topic, subscriptions M.! topic)) msg
      result <- try (sendPacket sock pkt) :: IO (Either SomeException ())
      let pending' | qos /= Zero = m : pending 
                    | otherwise = pending 
          conn' | isLeft result = Nothing
                | otherwise = conn
          session' = Session clientId subscriptions keepAlive will pending' conn' listenThread
      return session'
    _ -> return s

-- | Creates one Publish packet for each connected subscription
createMessagePackets :: Message -> [((Topic, QoS), Maybe Socket)] -> [(Packet, Socket)]
createMessagePackets (Message _ msg pid) subConns = map (first createPacket) connectedSubs
  where 
    connectedSubs :: [((Topic, QoS), Socket)]
    connectedSubs = mapMaybe (\(sub, mConn) -> mConn >>= (\conn -> return (sub, conn))) subConns

    createPacket :: (Topic, QoS) -> Packet
    createPacket sub = writePublishPacket pid (PublishFlags False False sub) msg

sessionHandler :: MVar [Session] -> Int -> IO ()
sessionHandler sessions ms = do
    threadDelay (ms * 1000)
    _ <- readMVar sessions >>= mapM_ (\s -> mapM_ (\x -> sendMessage x True s) (pending s))
    sessionHandler sessions ms

acceptClientLoop :: Socket -> Token -> MVar (Queue Message) -> MVar [Session] -> IO ()
acceptClientLoop broker sSecret queue sessions = do
    (client, _) <- accept broker
    _ <- forkIO $ connectClient client sSecret queue sessions
    acceptClientLoop broker sSecret queue sessions

connectClient :: Socket -> Token -> MVar (Queue Message) -> MVar [Session] -> IO ()
connectClient sock sSecret queue sessions = do
    conResp <- handleConnect sock sSecret sessions
    subResp <- handleSubscribe sock
    case (conResp, subResp) of
        (Just session, Just subscriptions) -> do
          let cid = clientId session
              -- TODO move to Broker.MVar (?)
              pushSession :: Session -> IO ()
              pushSession session = modifyMVar_ sessions $ return . (session :) . (filter (\s -> clientId s /= cid))
          pushSession (session `addSubs` subscriptions)
          putStrLn $ "Connected with client: " ++ cid
          listenToClient sock queue sessions
        _ -> return ()

-- TODO move to Broker.Session. Also, should probably implement this using a lens
addSubs :: Session -> [(Topic, QoS)] -> Session
addSubs (Session cid subs ka will pending conn tid) subscriptions = Session cid (subs `M.union` M.fromList subscriptions) ka will pending conn tid

handleConnect :: Socket -> Token -> MVar [Session] -> IO (Maybe Session)
handleConnect sock sSecret sessions = do
    connectPacket <- recvPacket sock >>= (\case {Just x -> return $ readConnectPacket x; Nothing -> return Nothing})
    case connectPacket of
      Nothing -> Nothing <$ sendPacket sock (writeConnackPacket False BadProtocalError)
      Just (cid, ConnectFlags _ cSecret will cleanSession, keepAlive) -> do
        case authCheck sSecret cSecret of -- If multiple checks need to be done, implement them here with the return of an ConnackResponse
          Accepted -> do
            session <- filter (\s -> clientId s == cid) <$> readMVar sessions
            case (session, cleanSession) of
              ([Session _ subs _ w pending oldSock tid], False) -> do 
                maybe (return ()) (`sendPacket` writeDisconnectPacket) oldSock -- Disconnect the old socket
                maybe (return ()) close oldSock
                killThread tid -- Kill old listening thread
                sendPacket sock $ writeConnackPacket True Accepted
                Just . Session cid subs keepAlive (w <|> will) pending (Just sock) <$> myThreadId
              _ -> do
                sendPacket sock $ writeConnackPacket False Accepted
                Just . Session cid M.empty keepAlive will [] (Just sock) <$> myThreadId
          connErr -> Nothing <$ sendPacket sock (writeConnackPacket False connErr)

authCheck :: Token -> Token -> ConnackResponse
authCheck (Just a) (Just b)   = if a == b then Accepted else AuthError
authCheck (Just _) _          = BadAuthError
authCheck _ (Just _)          = Accepted
authCheck _ _                 = Accepted

handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe sock = do
    subscriptions <- recvPacket sock >>= (\case {Just x -> return $ readSubscribePacket x; Nothing -> return Nothing})
    case subscriptions of
      Nothing -> return Nothing
      Just (pid, subs) -> 
        let pkt = writeSubackPacket pid (map (Just . snd) subs)
        in Just subs <$ sendPacket sock pkt

listenToClient :: Socket -> MVar (Queue Message) -> MVar [Session] -> IO ()
listenToClient sock queue sessions = do
    resp <- recvPacket sock
    tid <- myThreadId
    -- Spawn extra fork to assist in situation when client spams messages. (Does not guarantee delivery of Qos0 per specification)
    _ <- forkIO $ handleResponse resp sock queue sessions tid
    listenToClient sock queue sessions

handleResponse :: Maybe Packet -> Socket -> MVar (Queue Message) -> MVar [Session] -> ThreadId -> IO ()
handleResponse resp sock queue sessions tid 
  | Just packet <- resp = case cmd packet of
    PUBLISH -> handlePublish packet sock queue
    PUBACK -> handlePuback packet sock sessions
    DISCONNECT -> do
      -- When a client sends a disconnect message we assume the client wants the session deleted.
      -- Could be extended in future work.
      session <- head <$> (filter (\s -> conn s == Just sock) <$> readMVar sessions)
      putStrLn $ "Received close from: " ++ clientId session 
      modifyMVar_ sessions (return . filter (\s -> conn s /= Just sock)) 
      close sock 
      killThread tid -- Thread should die here
    _ -> return ()
  | otherwise = return ()

handlePublish :: Packet -> Socket -> MVar (Queue Message) -> IO ()
handlePublish p sock queue = case readPublishPacket p of
    Nothing -> return ()
    Just (pid, PublishFlags _ _ (topic, qos), msgStr) -> do
      handleIncomingQoS qos pid sock
      let message = Message topic msgStr pid
      updateMVar queue (single message) (return . push message)

updateMVar :: MVar a -> a -> (a -> IO a) -> IO ()
updateMVar v a f = do
  isEmpty <- isEmptyMVar v
  if isEmpty then putMVar v a
  else modifyMVar_ v f

handleIncomingQoS :: QoS -> PacketId -> Socket -> IO ()
handleIncomingQoS Zero _ _ = return ()
handleIncomingQoS One pid sock = sendPacket sock $ writePubackPacket pid
handleIncomingQoS Two _ _ = error "QoS2 is not supported within this broker"

handlePuback :: Packet -> Socket -> MVar [Session] -> IO ()
handlePuback packet sock sessions = do
    modifyMVar_ sessions $ mapM $ \s@(Session {..}) -> (
        if Just sock == conn then do
            return $ Session clientId subscriptions keepAlive will [msg | msg <- pending, readPacketId packet /= Just (pid msg)] conn listenThread
        else return s)
