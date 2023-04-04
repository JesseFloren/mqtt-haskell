module Spec.QoS0.Broker where

import Arbitrary
import Broker
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

test :: TestTree 
test = testGroup "Broker" [test_createMessagePackets]

test_createMessagePackets :: TestTree
test_createMessagePackets = testGroup "createMessagePackets" [shouldIgnoreNothings]
  where
    shouldIgnoreNothings = QC.testProperty "ignores Nothings" (\m subs -> isEmpty $ createMessagePackets m $ map (\sub -> (sub, Nothing)) subs)

    isEmpty xs = length xs QC.=== 0