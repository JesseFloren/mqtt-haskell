module Packets.ConnackResponse (ConnackResponse(..), toInt, fromInt) where

import qualified Data.Map as M
import Utils.Map ( toB, fromB )

data ConnackResponse =
    Accepted
  | BadProtocalError
  | BadClientIdError
  | UnavailableError
  | BadAuthError
  | AuthError
  deriving (Ord, Eq, Show)

mapConnackResponse :: M.Map ConnackResponse Int
mapConnackResponse = M.fromList [
    (Accepted, 0)
  , (BadProtocalError, 1)
  , (BadClientIdError, 2)
  , (UnavailableError, 3)
  , (BadAuthError, 4)
  , (AuthError, 5)
  ]

toInt :: ConnackResponse -> Int
toInt = toB mapConnackResponse

fromInt :: Int -> ConnackResponse
fromInt = fromB mapConnackResponse
