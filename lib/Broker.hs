{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Broker where

import Network.Socket ( Socket, PortNumber, accept )
import Control.Concurrent
import Socket.Base (createServer, recvPacket, sendPacket)
import Utils.Queue ( Queue (..), pop, push, single )
import Control.Applicative ( Alternative((<|>)) )
import Packets
import qualified Data.Map as M

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId, sThread :: ThreadId }

data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will :: Maybe (Retain, QoS, Topic, String),
    pending :: [Message],
    conn :: Maybe Socket}

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
            modifyMVar_ sessions (mapM (sendMessage msg))
            case queue of
                End -> messageQueue messages sessions
                _ -> do
                    modifyMVar_ messages (\_ -> return queue)
                    messageQueue messages sessions

sendMessage :: Message -> Session -> IO Session
sendMessage m@(Message topic msg pid) s@(Session {..}) = case (conn, M.member topic subscriptions) of
    (Just sock, True) -> do
        let qos = subscriptions M.! topic
        sendPacket sock $ writePublishPacket pid (PublishFlags False False (topic, subscriptions M.! topic)) msg
        return $ Session clientId subscriptions keepAlive will (if qos /= Zero then m:pending else pending) conn
    _ -> return s

sessionHandler :: MVar [Session] -> Int -> IO ()
sessionHandler sessions ms = do
    threadDelay (ms * 1000)
    _ <- readMVar sessions >>= mapM_ (\s -> mapM_ (`sendMessage` s) (pending s))
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
        (Just (Session cid subs ka will pending conn), Just subscriptions) -> do
            modifyMVar_ sessions $ return . (Session cid (subs `M.union` M.fromList subscriptions) ka will pending conn:)
            putStrLn $ "Connected with client: " ++ cid
            listenToClient sock queue sessions
        _ -> return ()

handleConnect :: Socket -> Token -> MVar [Session] -> IO (Maybe Session)
handleConnect sock sSecret sessions = do
    connectPacket <- recvPacket sock >>= (\case {Just x -> return $ readConnectPacket x; Nothing -> return Nothing})
    case connectPacket of
        Nothing -> do
            sendPacket sock $ writeConnackPacket False BadProtocalError
            return Nothing
        Just (cid, ConnectFlags _ cSecret will cleanSession, keepAlive) -> do
            -- Implement authentication
            (if isAuthenticated sSecret cSecret
              then (do
                session <- filter (\s -> clientId s == cid) <$> readMVar sessions
                case (session, cleanSession) of
                  ([Session _ subs _ w pending _], False) -> do
                      sendPacket sock $ writeConnackPacket True Accepted
                      return $ Just $ Session cid subs keepAlive (w <|> will) pending (Just sock)
                  _ -> do
                      sendPacket sock $ writeConnackPacket False Accepted
                      return $ Just $ Session cid M.empty keepAlive will [] (Just sock))
              else (do
                sendPacket sock $ writeConnackPacket False AuthError
                return Nothing))


isAuthenticated :: Token -> Token -> Bool
isAuthenticated sSecret cSecret = do
    case (sSecret, cSecret) of
        (Just a, Just b)    -> a == b
        (Just a, _)         -> False
        (_ , Just b)        -> True
        (_, _)              -> True

handleSubscribe :: Socket -> IO (Maybe [(Topic, QoS)])
handleSubscribe sock = do
    subscriptions <- recvPacket sock >>= (\case {Just x -> return $ readSubscribePacket x; Nothing -> return Nothing})
    case subscriptions of
        Nothing -> do
            return Nothing
        Just (pid, subs) -> do
            sendPacket sock $ writeSubackPacket pid (map (Just . snd) subs)
            return $ Just subs

listenToClient :: Socket -> MVar (Queue Message) -> MVar [Session] -> IO ()
listenToClient sock queue sessions = do
    resp <- recvPacket sock
    case resp of
        Nothing -> return ()
        Just packet -> do
            case cmd packet of
                PUBLISH -> handlePublish packet sock queue
                PUBACK -> handlePuback packet sock sessions
                _ -> return ()
            listenToClient sock queue sessions

handlePublish :: Packet -> Socket -> MVar (Queue Message) -> IO ()
handlePublish p sock queue = case readPublishPacket p of
    Nothing -> return ()
    Just (pid, PublishFlags _ _ (topic, qos), msg) -> do
        handleIncommingQoS qos pid sock
        isempty <- isEmptyMVar queue
        if isempty then putMVar queue (single (Message topic msg pid))
        else modifyMVar_ queue (return . push (Message topic msg pid))

handleIncommingQoS :: QoS -> PacketId -> Socket -> IO ()
handleIncommingQoS Zero _ _ = return ()
handleIncommingQoS One pid sock = sendPacket sock $ writePubackPacket pid
handleIncommingQoS Two _ _ = error "QoS2 is not supported within this broker"

handlePuback :: Packet -> Socket -> MVar [Session] -> IO ()
handlePuback packet sock sessions = do
    modifyMVar_ sessions $ mapM $ \s@(Session {..}) -> (
        if Just sock == conn then do
            return $ Session clientId subscriptions keepAlive will [msg | msg <- pending, readPacketId packet /= Just (pid msg)] conn
        else return s)