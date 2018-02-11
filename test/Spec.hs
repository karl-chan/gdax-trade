import qualified Gdax.Test.Integration as Integration
import qualified Gdax.Test.Unit        as Unit

import           Gdax.Test.Data
import           Gdax.Util.Config
import           Gdax.Util.Logger

import           Test.Tasty

main :: IO ()
main = withGlobalLogging (logConf testConfig) $ do defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [Unit.tests, Integration.tests]
