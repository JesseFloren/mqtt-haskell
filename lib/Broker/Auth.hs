{-# LANGUAGE RecordWildCards #-}

module Broker.Auth where
import Broker.State (AuthToken, BrokerState (..), BrokerConfig (sSecret))
import Packets.Abstract (ClientId, ConnectFlags(password), KeepAlive)
import Packets.ConnackResponse (ConnackResponse(..))

authCheck :: AuthToken -> AuthToken -> ConnackResponse
authCheck (Just a) (Just b)   = if a == b then Accepted else AuthError
authCheck (Just _) _          = BadAuthError
authCheck _ (Just _)          = Accepted
authCheck _ _                 = Accepted

--- Performs validation of packet
validatePacket :: Maybe (ClientId, ConnectFlags, KeepAlive) -> BrokerState -> ConnackResponse
validatePacket m BrokerState{..} = maybe BadProtocalError (\(_, flags, _) -> authCheck (sSecret config) (password flags)) m