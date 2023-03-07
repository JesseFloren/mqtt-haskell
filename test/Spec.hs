import Test.Tasty (defaultMain, testGroup)
import qualified Spec.Sanity as Sanity


main :: IO ()
main = defaultMain $ testGroup "mqtt-hs" [Sanity.test]