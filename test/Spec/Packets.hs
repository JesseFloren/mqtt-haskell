module Spec.Packets where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Packets
import Arbitrary.Packets ()

test :: TestTree
test = testGroup "Packets" [readWriteTests, encodeDecodeTests]

--- *** Read & Write tests *** ---
readWriteTests :: TestTree
readWriteTests = testGroup "Write & Read packets" [readWriteConnect, readWriteConnack, readWritePublish, readWritePuback, 
                                                   readWritePubrec, readWritePubrel, readWritePubcomp, readWriteSubscribe,
                                                   readWriteSuback, readWriteUnsubscribe, readWriteSubscribe]

readWriteConnect :: TestTree
readWriteConnect = QC.testProperty "Connect Packet" readWrite where
    readWrite x y z = readConnectPacket (writeConnectPacket x y z) == Just (x, y, z)

readWriteConnack :: TestTree
readWriteConnack = QC.testProperty "Connack Packet" readWrite where
    readWrite x y = readConnackPacket (writeConnackPacket x y) == Just (x, y)

readWritePublish :: TestTree
readWritePublish = QC.testProperty "Publish Packet" readWrite where
    readWrite x y z = readPublishPacket (writePublishPacket x y z) == Just (x, y, z)

readWritePuback :: TestTree
readWritePuback = QC.testProperty "Puback Packet" readWrite where
    readWrite x = readPacketId (writePubackPacket x) == Just x

readWritePubrec :: TestTree
readWritePubrec = QC.testProperty "Pubrec Packet" readWrite where
    readWrite x = readPacketId (writePubrecPacket x) == Just x

readWritePubrel :: TestTree
readWritePubrel = QC.testProperty "Pubrel Packet" readWrite where
    readWrite x = readPacketId (writePubrelPacket x) == Just x

readWritePubcomp :: TestTree
readWritePubcomp = QC.testProperty "Pubcomp Packet" readWrite where
    readWrite x = readPacketId (writePubcompPacket x) == Just x

readWriteSubscribe :: TestTree
readWriteSubscribe = QC.testProperty "Subscribe Packet" readWrite where
    readWrite x y = readSubscribePacket (writeSubscribePacket x y) == Just (x, y)

readWriteSuback :: TestTree
readWriteSuback = QC.testProperty "Suback Packet" readWrite where
    readWrite x y = readSubackPacket (writeSubackPacket x y) == Just (x, y)

readWriteUnsubscribe :: TestTree
readWriteUnsubscribe = QC.testProperty "Unsubscribe Packet" readWrite where
    readWrite x y = readUnsubscribePacket (writeUnsubscribePacket x y) == Just (x, y)

readWriteUnsuback :: TestTree
readWriteUnsuback = QC.testProperty "Unsuback Packet" readWrite where
    readWrite x = readPacketId (writeUnSubackPacket x) == Just x

--- *** Encode & Decode tests *** ---
encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encode & Decode packets" [encodeDecodeTest]

encodeDecodeTest :: TestTree
encodeDecodeTest = QC.testProperty "Arbitrary Packets" encodeDecode where
    encodeDecode p = byteStringToPacket (packetToByteString p) == Just p