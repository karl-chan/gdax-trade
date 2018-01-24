import           Gdax.Algo.Master
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Time

import           Control.Monad.Reader
import           Data.Time.Clock
import           Prelude              hiding (product)

main :: IO ()
main = do
  let product = Pair ETH EUR
  config <- getGlobalConfig
  withGlobalLogging (logConf config) $ do
    now <- getCurrentTime
    let startTime = addUTCTime (-day) now
    runReaderT (master [product] startTime) config
