import           Gdax.Algo.Master
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Logger

import           Control.Monad.Reader
import           Prelude              hiding (product)

main :: IO ()
main = do
  let product = Pair ETH EUR
  config <- getGlobalConfig
  withGlobalLogging (logConf config) $ do runReaderT (master [product]) config
