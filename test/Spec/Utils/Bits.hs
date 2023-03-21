module Spec.Utils.Bits where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

import Utils.Bits

test :: TestTree
test = testGroup "Utils.Bits" tests

tests :: [TestTree]
tests = [testIntegralToBits]

testIntegralToBits :: TestTree
testIntegralToBits  = testGroup "integralToBits" tests'
  where
    tests' = [
        QC.testProperty "implementation is equal to intToBits" (\i -> isInt4 i QC.==> (intToBits 4 i QC.=== bitsToBitList 4 i))
      ]

    isInt4 :: Int -> Bool
    isInt4 i = i >= 0 && i < 16

