module Broker
  (
    createBroker, BrokerConfig(..), Message(..)
  )
  where

import Broker.Base ( createBroker )
import Broker.State ( BrokerConfig(..), Message(..) )