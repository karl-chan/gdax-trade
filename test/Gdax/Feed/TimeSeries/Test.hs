module Gdax.Feed.TimeSeries.Test where

import           Gdax.Feed.Gdax
import           Gdax.Feed.TimeSeries
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader
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
  tsFeed <- runReaderT (newTimeSeriesFeed gdaxFeed lastHour product) config
  forever $ do
    series <- readFeed tsFeed
    print series
