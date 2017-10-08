{-# LANGUAGE OverloadedStrings #-}

import qualified Gdax.Data.OrderBook.Test     as OrderBook
import           Gdax.Util.Feed.TimeSeries
import qualified Gdax.Data.TimeSeries.Test    as TimeSeries
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Config

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

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
