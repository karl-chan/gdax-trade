module Gdax.Data.TimeSeries.Test where

import           Gdax.Util.Feed.TimeSeries
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Control.Monad.Reader
import           Data.Time.Calendar
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit

test :: Granularity -> Product -> GdaxFeed -> Config -> TestTree
test granularity product gdaxFeed config = do
    testGroup
        "Time Series"
        [ testCase "Check that time series is consistent with GDAX records" $
          testImplementation granularity product gdaxFeed config
        ]

testImplementation :: Granularity -> Product -> GdaxFeed -> Config -> Assertion
testImplementation granularity product gdaxFeed config = do
    feedListener <- newFeedListener gdaxFeed
    now <- getCurrentTime
    let lastHour = addUTCTime (-3600) now
    void $ runReaderT (newTimeSeriesFeed gdaxFeed lastHour product) config
