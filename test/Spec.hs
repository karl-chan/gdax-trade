module Main where

import           Gdax.Feed.Gdax
import qualified Gdax.Feed.OrderBook.Test     as OrderBook
import           Gdax.Feed.TimeSeries
import qualified Gdax.Feed.TimeSeries.Test    as TimeSeries
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries
import           Gdax.Util.Config

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Control.Monad.Reader
import           Test.Tasty

granularity :: Granularity
granularity = 60 -- 60 seconds

testProduct :: Product
testProduct = Pair BTC USD

main :: IO ()
main = do
    testConfig <- getGlobalConfig
    testFeed <- runReaderT (newGdaxFeed [testProduct]) testConfig
    defaultMain $ tests testProduct testFeed testConfig

tests :: Product -> GdaxFeed -> Config -> TestTree
tests product gdaxFeed config = testGroup "All tests" [OrderBook.test product gdaxFeed config]
--        TimeSeries.test granularity product gdaxFeed config]
