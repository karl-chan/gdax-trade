module Gdax.Test.Integration.Feed.TimeSeries where

import           Gdax.Test.Data

import           Gdax.Feed.TimeSeries
import qualified Gdax.Types.TimeSeries.Util as TS
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Time

import           Control.Monad.Reader
import           Data.Time.Clock
import           Prelude                    hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Time Series" test

test :: Assertion
test = do
  now <- getCurrentTime
  tsFeed <- runReaderT (newTimeSeriesFeed testProduct testGdaxFeed) testConfig
  tsFeedListener <- newFeedListener tsFeed
  initialSeries <- readFeed tsFeedListener
  let (initialStart, initialEnd) = TS.range initialSeries
      loop = do
        series <- readFeed tsFeedListener
        if series /= initialSeries
          then do
            let (start, end) = TS.range series
                margin = 5 * minute
                window = initialPeriod . timeSeriesConf $ testConfig
                startOfWindow = addUTCTime (-window) now
            assertBool
              ("Start time: " ++
               show start ++
               " should be close to start of window: " ++ show startOfWindow)
              (diffUTCTime start startOfWindow < margin)
            assertBool
              ("End time: " ++
               show end ++ " should be close to now: " ++ show now)
              (diffUTCTime now end < margin)
            assertEqual
              "Start time should be equal to initial start time"
              initialStart
              start
            assertBool
              ("End time: " ++
               show end ++
               " should be greater than initial end time: " ++ show initialEnd) $
              end > initialEnd
          else loop
  loop
