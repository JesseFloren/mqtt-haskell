module Spec.QoS0.Broker where

import Arbitrary ()
import Broker
import Network.Socket ( Socket )
import Test.Tasty (TestTree, testGroup)

import Data.Maybe (isJust)

import qualified Test.Tasty.QuickCheck as QC

test :: TestTree 
test = testGroup "Broker" [test_createMessagePackets]

instance QC.Arbitrary Socket where
  arbitrary = undefined


test_createMessagePackets :: TestTree
test_createMessagePackets = testGroup "createMessagePackets" [shouldIgnoreNothings]
  where
    shouldIgnoreNothings = QC.testProperty "filters out unconnected subscriptions" 
                                           (\m subs -> let justCount = length (filter (isJust . snd) subs)
                                                       in length (createMessagePackets m subs) QC.=== justCount)
