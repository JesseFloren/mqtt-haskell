import Test.Tasty (defaultMain, testGroup)
import qualified Spec.Sanity as Sanity
import qualified Spec.Packets as Packets
import qualified Spec.Utils.Bits as Bits


main :: IO ()
main = defaultMain $ testGroup "mqtt-hs" [
    Sanity.test
  , Packets.test
  , Bits.test
  ]
