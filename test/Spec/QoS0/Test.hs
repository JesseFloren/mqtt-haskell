module Spec.QoS0.Test where

import Test.Tasty (TestTree, testGroup)
import qualified Spec.QoS0.Broker as Broker
import qualified Spec.QoS0.Client as Client

test :: TestTree 
test = testGroup "QoS 0" [Broker.test, Client.test]