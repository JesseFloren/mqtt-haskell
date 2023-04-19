{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arbitrary.Packets where
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), oneof, Gen, elements, listOf, listOf1)
import Packets 
import Arbitrary.QoS ()

genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['0' .. '9']

genSafeString :: Gen String
genSafeString = listOf genSafeChar    

genSafeInt :: Gen Int
genSafeInt = elements [0..128]

instance Arbitrary Packet where
    arbitrary = oneof [arbitraryConnack, arbitraryConnect, arbitraryDisconnect, arbitraryPingreq, arbitraryPingresp, arbitraryPuback
                      , arbitraryPubcomp, arbitraryPublish, arbitraryPubrec, arbitraryPubrel, arbitrarySuback, arbitrarySubscribe, 
                      arbitraryUnsuback, arbitraryUnsubscribe]

instance Arbitrary ConnectFlags where
    arbitrary = oneof [ConnectFlags <$> (Just <$> genSafeString) <*> (Just <$> genSafeString) <*> arbitraryWill <*> arbitrary,
                       ConnectFlags Nothing Nothing <$> arbitraryWill <*> arbitrary] where
                        arbitraryWill = oneof [(\a b c d -> Just (a,b,c,d)) <$> arbitrary <*> arbitrary <*> genSafeString <*> genSafeString, pure Nothing]

instance Arbitrary ConnackResponse where
    arbitrary = elements [Accepted, BadProtocalError, BadClientIdError, UnavailableError, BadAuthError, AuthError]

instance Arbitrary PublishFlags where
    arbitrary = PublishFlags <$> arbitrary <*> arbitrary <*> ((,) <$> genSafeString <*> arbitrary)

arbitraryConnect :: Gen Packet
arbitraryConnect = writeConnectPacket <$> genSafeString <*> arbitrary <*> genSafeInt

arbitraryConnack :: Gen Packet
arbitraryConnack = writeConnackPacket <$> arbitrary <*> arbitrary

arbitraryPublish :: Gen Packet
arbitraryPublish = writePublishPacket <$> genSafeInt <*> arbitrary <*> genSafeString

arbitraryPuback :: Gen Packet
arbitraryPuback = writePubackPacket <$> genSafeInt

arbitraryPubrec :: Gen Packet
arbitraryPubrec = writePubrecPacket <$> genSafeInt

arbitraryPubrel :: Gen Packet
arbitraryPubrel = writePubrelPacket <$> genSafeInt

arbitraryPubcomp :: Gen Packet
arbitraryPubcomp = writePubcompPacket <$> genSafeInt

arbitrarySubscribe :: Gen Packet
arbitrarySubscribe = writeSubscribePacket <$> genSafeInt <*> arbitraryTopics where
    arbitraryTopics = listOf1 ((,) <$> genSafeString <*> arbitrary)

arbitrarySuback :: Gen Packet
arbitrarySuback = writeSubackPacket <$> genSafeInt <*> arbitrary

arbitraryUnsubscribe :: Gen Packet
arbitraryUnsubscribe = writeUnsubscribePacket <$> genSafeInt <*> arbitraryTopics where
    arbitraryTopics = listOf1 ((,) <$> genSafeString <*> arbitrary)

arbitraryUnsuback :: Gen Packet
arbitraryUnsuback = writeUnSubackPacket <$> genSafeInt

arbitraryPingreq :: Gen Packet
arbitraryPingreq = pure writePingreqPacket

arbitraryPingresp :: Gen Packet
arbitraryPingresp = pure writePingrespPacket

arbitraryDisconnect :: Gen Packet
arbitraryDisconnect = pure writeDisconnectPacket
