module Spec.QoS0.Client where


import Arbitrary ()
import Client
import Network.Socket ( Socket )
import Test.Tasty (TestTree, testGroup)

import Data.Maybe (isJust)

import qualified Test.Tasty.QuickCheck as QC

test :: TestTree 
test = testGroup "Client" []
