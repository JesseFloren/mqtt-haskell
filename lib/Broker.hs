{-# LANGUAGE LambdaCase #-}
module Broker where

import Network.Socket
import Control.Concurrent
import Socket.Base (createServer, recvPacket, sendPacket)
import Packets.Abstract ( QoS, Topic, Packet (cmd), ConnectFlags (ConnectFlags), Retain, ConnackResponse (Accepted, BadProtocalError), CommandType (..), PacketId, PublishFlags (PublishFlags) )
import Utils.Queue ( Queue (..), pop, push, single )
import Packets.Simple (readConnectPacket, writeConnackPacket, readSubscribePacket, writeSubackPacket, writePublishPacket, readPublishPacket)
import Control.Applicative
import qualified Data.Map as M

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId }

data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will:: Maybe (Retain, QoS, Topic, String),
    conn :: Maybe Socket}

data Message = Message {topic :: Topic, msg :: String, pid :: PacketId} deriving (Show)

createBroker :: PortNumber -> IO MqttBroker
createBroker port = do
    sock <- createServer port
    sessions <- newMVar []
    messages <- newEmptyMVar
    aThread <- forkIO $ acceptClientLoop sock messages sessions
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
sendMessage _ [] = return ()
sendMessage m ((_, Nothing):xs) = sendMessage m xs
sendMessage m@(Message _ msg pid) ((sub, Just conn):xs) = do
    sendPacket conn $ writePublishPacket pid (PublishFlags False False sub) msg
    sendMessage m xs

acceptClientLoop :: Socket ->  MVar (Queue Message) -> MVar [Session] -> IO ()
acceptClientLoop broker queue sessions = do
    (client, _) <- accept broker
    _ <- forkIO $ connectClient client queue sessions
    acceptClientLoop broker queue sessions

connectClient :: Socket -> MVar (Queue Message) -> MVar [Session] -> IO ()
connectClient sock queue sessions = do
    conResp <- handleConnect sock sessions
    subResp <- handleSubscribe sock
    case (conResp, subResp) of
        (Just (Session cid subs ka will conn), Just subscriptions) -> do
            modifyMVar_ sessions $ return . (Session cid (subs `M.union` M.fromList subscriptions) ka will conn:)
            putStrLn $ "Connected with client: " ++ cid
            listenToClient sock queue
        _ -> return ()

handleConnect :: Socket -> MVar [Session] -> IO (Maybe Session)
handleConnect sock sessions = do
    connectPacket <- recvPacket sock >>= (\case {Just x -> return $ readConnectPacket x; Nothing -> return Nothing})
    case connectPacket of 
        Nothing -> do
            sendPacket sock $ writeConnackPacket True BadProtocalError
            return Nothing
        Just (cid, ConnectFlags user pass will cleanSession, keepAlive) -> do
            -- Implement authentication
            session <- filter (\s -> clientId s == cid) <$> readMVar sessions
            case (session, cleanSession) of
                ([Session _ subs _ w _], False) -> do
                    sendPacket sock $ writeConnackPacket True Accepted
                    return $ Just $ Session cid subs keepAlive (w <|> will) (Just sock)
                _ -> do
                    sendPacket sock $ writeConnackPacket False Accepted
                    return $ Just $ Session cid M.empty keepAlive will (Just sock)


handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe sock = do
    subscriptions <- recvPacket sock >>= (\case {Just x -> return $ readSubscribePacket x; Nothing -> return Nothing})
    case subscriptions of
        Nothing -> do
            return Nothing
        Just (pid, subs) -> do
            sendPacket sock $ writeSubackPacket pid (map (Just . snd) subs)
            return $ Just subs

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
    Just (pid, PublishFlags _ _ (topic, _), msg) -> do
        putStrLn $ "Received " ++ topic ++ ": " ++ msg
        isempty <- isEmptyMVar queue
        if isempty then putMVar queue (single (Message topic msg pid))
        else modifyMVar_ queue (return . push (Message topic msg pid))   
