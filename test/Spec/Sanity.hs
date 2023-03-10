{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Spec.Sanity(test) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>))

test :: TestTree
test = testGroup "Sanity" [hunitCheck, quickCheckCheck]

hunitCheck :: TestTree
hunitCheck = testCase "HUnit sanity check" (True @?= True)

quickCheckCheck :: TestTree
quickCheckCheck = QC.testProperty "QuickCheck sanity check" (\b -> b ==> True)