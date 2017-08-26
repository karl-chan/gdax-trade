{-# LANGUAGE OverloadedStrings #-}

import qualified Gdax.Data.OrderBook.Test     as OrderBook
import           Gdax.Data.TimeSeries
import qualified Gdax.Data.TimeSeries.Test    as TimeSeries
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
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
    testFeed <- newProductFeed [testProduct]
    defaultMain $ tests testProduct testFeed testConfig

tests :: Product -> ProductFeed -> Config -> TestTree
tests product productFeed config = testGroup "All tests" [OrderBook.test product productFeed config]
--        TimeSeries.test granularity product productFeed config]
