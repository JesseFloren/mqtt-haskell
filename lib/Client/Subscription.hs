module Client.Subscription where

import Client.Connection (ConnAction)
import Packets (Topic)
import qualified Data.Map as M
import qualified Data.Set as S


type Handler = ConnAction (String -> IO ())
type Subscription = M.Map Topic Handler

-- *** Subscriptions DSL *** --
sub :: Topic -> Handler -> Subscription
sub = M.singleton

subGroup :: [Subscription] -> Subscription
subGroup = foldr M.union M.empty

topics :: Subscription -> S.Set Topic
topics = M.keysSet

findHandler :: Subscription -> Topic -> Handler
findHandler = (M.!)

empty :: Subscription
empty = M.empty
