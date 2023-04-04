module Arbitrary.QoS where

import Packets (QoS(..))
import qualified Test.Tasty.QuickCheck as QC

instance QC.Arbitrary QoS where
  arbitrary = QC.elements [Zero, One, Two]
