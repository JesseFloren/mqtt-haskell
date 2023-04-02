module Client.Subscription where

import Client.Connection (ConnAction)
import Packets (Topic)
import qualified Data.Map as M


type Handler = ConnAction (String -> IO ())
type Subscription = M.Map Topic Handler

--- *** Build Subs *** ---
sub :: Topic -> Handler -> Subscription
sub = M.singleton

subGroup :: [Subscription] -> Subscription
subGroup = foldr M.union M.empty

topics :: Subscription -> [Topic]
topics = M.keys

findHandler :: Subscription -> Topic -> Handler
findHandler = (M.!)

empty :: Subscription
empty = M.empty
