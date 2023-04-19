import Test.Tasty (defaultMain, testGroup)
import qualified Spec.Sanity as Sanity
import qualified Spec.Packets as Packets
import qualified Spec.QoS0.Test as QoS0


main :: IO ()
main = defaultMain $ testGroup "mqtt-hs" [
    Sanity.test
  , Packets.test
  , QoS0.test
  ]
