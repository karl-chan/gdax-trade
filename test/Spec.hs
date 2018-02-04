import qualified Gdax.Test.Integration as Integration
import qualified Gdax.Test.Unit        as Unit
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [Unit.tests, Integration.tests]
