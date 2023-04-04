module Spec.QoS0.Test where

import Test.Tasty (TestTree, testGroup)
import qualified Spec.QoS0.Broker as Broker

test :: TestTree 
test = testGroup "QoS 0" [Broker.test]