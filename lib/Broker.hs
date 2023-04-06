{-# LANGUAGE LambdaCase #-}

module Broker where

import Network.Socket ( Socket, PortNumber, accept )
import Control.Concurrent 
import Socket.Base (createServer, recvPacket, sendPacket)
import Utils.Queue ( Queue (..), pop, push, single )
import Control.Applicative ( Alternative((<|>)) )
import Packets
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import qualified Data.Map as M
-- import Debug.Trace (trace, traceShow)

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId }

-- TODO move to Broker.Session
data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will:: Maybe (Retain, QoS, Topic, String),
    conn :: Maybe Socket
}

data Message = Message {topic :: Topic, msg :: String, pid :: PacketId} deriving (Show)

type Token = Maybe String

createBroker :: PortNumber -> Token -> IO MqttBroker
createBroker port sSecret = do
    sock <- createServer port
    sessions <- newMVar []
    messages <- newEmptyMVar
    aThread <- forkIO $ acceptClientLoop sock sSecret messages sessions
    mThread <- forkIO $ messageQueue messages sessions
    return $ MqttBroker sock aThread mThread

messageQueue :: MVar (Queue Message) -> MVar [Session] -> IO ()
messageQueue messages sessions = do
    (message, queue) <- pop <$> takeMVar messages
    case message of
        Nothing -> messageQueue messages sessions
        Just m@(Message topic _ _) -> do
            clients <- readMVar sessions
            sendMessage m [((topic, subs M.! topic), conn) | (Session _ subs _ _ conn) <- clients, M.member topic subs]
            case queue of
                End -> messageQueue messages sessions
                _ -> do
                    modifyMVar_ messages (\_ -> return queue)
                    messageQueue messages sessions

sendMessage :: Message -> [((Topic, QoS), Maybe Socket)] -> IO ()
sendMessage msg xs = 
  let pktConns = createMessagePackets msg xs
      send' (pkt, conn) = sendPacket conn pkt
  in mapM_ send' pktConns

-- | Creates one Publish packet for each connected subscription
createMessagePackets :: Message -> [((Topic, QoS), Maybe Socket)] -> [(Packet, Socket)]
createMessagePackets (Message _ msg pid) subConns = map (first createPacket) connectedSubs
  where 
    connectedSubs :: [((Topic, QoS), Socket)]
    connectedSubs = mapMaybe (\(sub, mConn) -> mConn >>= (\conn -> return (sub, conn))) subConns

    createPacket :: (Topic, QoS) -> Packet
    createPacket sub = writePublishPacket pid (PublishFlags False False sub) msg


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
          pushSession (session `addSubs` subscriptions)
          putStrLn $ "Connected with client: " ++ clientId session
          listenToClient sock queue
        _ -> return ()
  where 
    -- TODO move to Broker.MVar (?)
    pushSession :: Session -> IO ()
    pushSession session = modifyMVar_ sessions $ return . (session :)

-- TODO move to Broker.Session. Also, should probably implement this using a lens
addSubs :: Session -> [(Topic, QoS)] -> Session
addSubs (Session cid subs ka will conn) subscriptions = Session cid (subs `M.union` M.fromList subscriptions) ka will conn

handleConnect :: Socket -> Token -> MVar [Session] -> IO (Maybe Session)
handleConnect sock sSecret sessions = do
  connectPacket <- recvPacket sock >>= (\case {Just x -> return $ readConnectPacket x; Nothing -> return Nothing})
  case connectPacket of
    Nothing -> Nothing <$ (sendPacket sock $ writeConnackPacket False BadProtocalError)
    Just (cid, ConnectFlags _ cSecret will cleanSession, keepAlive)
      | not (isAuthenticated sSecret cSecret) -> Nothing <$ (sendPacket sock $ writeConnackPacket False AuthError)
      | otherwise -> do
        session <- filter (\s -> clientId s == cid) <$> readMVar sessions
        case (session, cleanSession) of
          ([Session _ subs _ w _], False) -> do
            sendPacket sock $ writeConnackPacket True Accepted
            return $ Just $ Session cid subs keepAlive (w <|> will) (Just sock)
          _ -> do
            sendPacket sock $ writeConnackPacket False Accepted
            return $ Just $ Session cid M.empty keepAlive will (Just sock)

isAuthenticated :: Token -> Token -> Bool
isAuthenticated sSecret cSecret = do
    case (sSecret, cSecret) of
        (Just a, Just b)    -> a == b
        (Just _, _)         -> False
        (_ , Just _)        -> True
        (_, _)              -> True

handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe sock = do
    subscriptions <- recvPacket sock >>= (\case {Just x -> return $ readSubscribePacket x; Nothing -> return Nothing})
    case subscriptions of
      Nothing -> return Nothing
      Just (pid, subs) -> 
        let pkt = writeSubackPacket pid (map (Just . snd) subs)
        in Just subs <$ sendPacket sock pkt

listenToClient :: Socket -> MVar (Queue Message) -> IO ()
listenToClient sock queue = do
    resp <- recvPacket sock
    case resp of
      Nothing -> return ()
      Just packet -> do
        case cmd packet of
          PUBLISH -> handlePublish packet queue
          _ -> return ()
        listenToClient sock queue

handlePublish :: Packet -> MVar (Queue Message) -> IO ()
handlePublish p queue = case readPublishPacket p of
  Nothing -> return ()
  Just (pid, PublishFlags _ _ (topic, _), msgStr) -> do
    let message = (Message topic msgStr pid)
    putStrLn $ "Received " ++ topic ++ ": " ++ msgStr
    updateMVar queue (single message) (return . push message)

updateMVar :: MVar a -> a -> (a -> IO a) -> IO ()
updateMVar v a f = do
  isEmpty <- isEmptyMVar v
  if isEmpty then putMVar v a
  else modifyMVar_ v f
