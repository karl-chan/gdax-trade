module Gdax.Test.Integration.Feed.TimeSeries where

import           Gdax.Test.Data

import           Gdax.Feed.Gdax
import           Gdax.Feed.TimeSeries
import           Gdax.Util.Feed

import           Control.Monad.Reader
import           Data.Time.Clock
import           Prelude              hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Time Series" [testSync]

testSync :: TestTree
testSync = do
  testCase "should match and sync with GDAX implementation" $ do
    now <- getCurrentTime
    let lastHour = addUTCTime (-3600) now
    gdaxFeed <- runReaderT (newGdaxFeed [testProduct]) testConfig
    tsFeed <-
      runReaderT (newTimeSeriesFeed lastHour gdaxFeed testProduct) testConfig
    forever $ do
      series <- readFeed tsFeed
      print series
