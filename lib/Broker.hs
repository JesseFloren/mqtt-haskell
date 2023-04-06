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
import qualified Data.Map as M
import Data.Either ( isLeft )

data MqttBroker = MqttBroker {socket :: Socket, acThread :: ThreadId,  mqThread :: ThreadId, sThread :: ThreadId }

data Session = Session {
    clientId :: String,
    subscriptions :: M.Map Topic QoS,
    keepAlive :: Int,
    will :: Maybe (Retain, QoS, Topic, String),
    pending :: [Message],
    conn :: Maybe Socket,
    listenThread :: ThreadId} deriving (Show)

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
        result <- try (sendPacket sock $ writePublishPacket pid (PublishFlags dup False (topic, subscriptions M.! topic)) msg) :: IO (Either SomeException ())
        return $ Session clientId subscriptions keepAlive will (if qos /= Zero then m:pending else pending) (if isLeft result then Nothing else conn) listenThread
    _ -> return s

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
        (Just (Session cid subs ka will pending conn tid), Just subscriptions) -> do
            let newSession = Session cid (subs `M.union` M.fromList subscriptions) ka will pending conn tid
            modifyMVar_ sessions (\x -> return $ newSession:filter (\s -> clientId s /= cid) x)
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

listenToClient :: Socket -> MVar (Queue Message) -> MVar [Session] -> IO ()
listenToClient sock queue sessions = do
    resp <- recvPacket sock -- Error handling here
    case resp of
        Nothing -> return ()
        Just packet -> do
            case cmd packet of
                PUBLISH -> do 
                    handlePublish packet sock queue
                    listenToClient sock queue sessions
                PUBACK -> do
                    handlePuback packet sock sessions
                    listenToClient sock queue sessions
                DISCONNECT -> do
                    session <- head <$> (filter (\s -> conn s == Just sock) <$> readMVar sessions)
                    putStrLn $ "Received close from: " ++ clientId session  
                    close sock
                    modifyMVar_ sessions (return . filter (\s -> conn s /= Just sock)) -- Thread should die here
                _ -> listenToClient sock queue sessions
            

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
            return $ Session clientId subscriptions keepAlive will [msg | msg <- pending, readPacketId packet /= Just (pid msg)] conn listenThread
        else return s)