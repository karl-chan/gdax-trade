module Main where

import           Gdax.Algo.Master
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Time

import           Control.Monad.Reader
import           Data.Time.Clock

main :: IO ()
main = do
    config <- getGlobalConfig
    withGlobalLogging (logConf config) $ do
        now <- getCurrentTime
        let startTime = addUTCTime (-day) now
        runReaderT (master [Pair ETH EUR] startTime) config
