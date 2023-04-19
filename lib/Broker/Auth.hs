module Broker.Auth where
import Packets.ConnackResponse (ConnackResponse(..))
import Broker.State (Session, BrokerAction, AuthToken)

authCheck :: AuthToken -> AuthToken -> ConnackResponse
authCheck (Just a) (Just b)   = if a == b then Accepted else AuthError
authCheck (Just _) _          = BadAuthError
authCheck _ (Just _)          = Accepted
authCheck _ _                 = Accepted
