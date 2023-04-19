module Arbitrary.Message where

import qualified Test.Tasty.QuickCheck as QC
import Broker

instance QC.Arbitrary Message where
  arbitrary = Message <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
