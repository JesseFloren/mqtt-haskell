module Spec.QoS0.Broker where

import Arbitrary ()
import Broker
import Network.Socket ( Socket )
import Test.Tasty (TestTree, testGroup)

import Data.Maybe (isJust)

import qualified Test.Tasty.QuickCheck as QC

test :: TestTree 
test = testGroup "Broker" []

instance QC.Arbitrary Socket where
  arbitrary = undefined
