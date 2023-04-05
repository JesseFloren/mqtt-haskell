{-# LANGUAGE LambdaCase #-}

module Broker where

import Network.Socket ( Socket, PortNumber, accept )
import Control.Concurrent
import Socket.Base (createServer, recvPacket, sendPacket)
import Utils.Queue ( Queue (..), pop, push, single )
import Control.Applicative ( Alternative((<|>)) )
import Packets
import qualified Data.Map as M
import Client.MqttConfig (CID)

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId }

data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will:: Maybe (Retain, QoS, Topic, String),
    conn :: Maybe Socket}

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
sendMessage _ [] = return ()
sendMessage m ((_, Nothing):xs) = sendMessage m xs
sendMessage m@(Message _ msg pid) ((sub, Just conn):xs) = do
    sendPacket conn $ writePublishPacket pid (PublishFlags False False sub) msg
    sendMessage m xs

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
        (Just (Session cid subs ka will conn), Just subscriptions) -> do
            modifyMVar_ sessions $ return . (Session cid (subs `M.union` M.fromList subscriptions) ka will conn:)
            putStrLn $ "Connected with client: " ++ cid
            listenToClient sock queue
        _ -> return ()

handleConnect :: Socket -> Token -> MVar [Session] -> IO (Maybe Session)
handleConnect sock sSecret sessions = do
    connectPacket <- recvPacket sock >>= (\case {Just x -> return $ readConnectPacket x; Nothing -> return Nothing})
    case connectPacket of
        Nothing -> do
            sendPacket sock $ writeConnackPacket False BadProtocalError
            return Nothing
        Just (cid, ConnectFlags _ cSecret will cleanSession, keepAlive) -> do
            case authCheck sSecret cSecret of -- If multiple checks need to be done, implement them here with the return of an error packet
                Accepted -> do
                    session <- filter (\s -> clientId s == cid) <$> readMVar sessions
                    case (session, cleanSession) of
                        ([Session _ subs _ w oldSock], False) -> do 
                            maybe (return ()) (`sendPacket` writeDisconnectPacket) oldSock -- Disconnect the old socket
                            sendPacket sock $ writeConnackPacket True Accepted
                            return $ Just $ Session cid subs keepAlive (w <|> will) (Just sock)
                        _ -> do
                            sendPacket sock $ writeConnackPacket False Accepted
                            return $ Just $ Session cid M.empty keepAlive will (Just sock)
                connErr -> do 
                    sendPacket sock $ writeConnackPacket False connErr
                    return Nothing

authCheck :: Token -> Token -> ConnackResponse
authCheck (Just a) (Just b)   = if a == b then Accepted else AuthError
authCheck (Just _) _          = BadAuthError
authCheck _ (Just _)          = Accepted
authCheck _ _                 = Accepted

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
