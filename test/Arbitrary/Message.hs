module Arbitrary.Message where

import Broker
import qualified Test.Tasty.QuickCheck as QC

instance QC.Arbitrary Message where
  arbitrary = Message <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
