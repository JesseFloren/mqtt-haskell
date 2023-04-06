{-# LANGUAGE TupleSections #-}

module Client.Subscription where

import Client.Connection (ConnAction)
import Packets (Topic, QoS)
import qualified Data.Map as M
import qualified Data.Set as S


type Handler = ConnAction (String -> IO ())
type Subscription = M.Map Topic (QoS, Handler)

-- *** Subscriptions DSL *** --
sub :: (Topic, QoS) -> Handler -> Subscription
sub (topic, qos) = M.singleton topic . (qos,)

subGroup :: [Subscription] -> Subscription
subGroup = foldr M.union M.empty

topics :: Subscription -> S.Set Topic
topics = M.keysSet

getSubs :: Subscription -> M.Map Topic QoS
getSubs = M.fromList . map (\(t, (q, _)) -> (t, q)) . M.toList

getHandler :: Subscription -> Topic -> Handler
getHandler subs topic = snd (subs M.! topic)

empty :: Subscription
empty = M.empty
