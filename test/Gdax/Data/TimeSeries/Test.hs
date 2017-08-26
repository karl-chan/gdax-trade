module Gdax.Data.TimeSeries.Test where

import           Gdax.Data.TimeSeries
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Control.Monad.Reader
import           Data.Time.Calendar
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit

test :: Granularity -> Product -> ProductFeed -> Config -> TestTree
test granularity product productFeed config = do
    testGroup
        "Time Series"
        [ testCase "Check that time series is consistent with GDAX records" $
          testImplementation granularity product productFeed config
        ]

testImplementation :: Granularity -> Product -> ProductFeed -> Config -> Assertion
testImplementation granularity product productFeed config = do
    feedListener <- newFeedListener productFeed >>= waitUntilFeed
    now <- getCurrentTime
    let lastHour = addUTCTime (-3600) now
    void $ runReaderT (liveTSFeed lastHour product productFeed) config
