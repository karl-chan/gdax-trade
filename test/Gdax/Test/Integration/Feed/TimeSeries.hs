module Gdax.Test.Integration.Feed.TimeSeries where

import           Gdax.Test.Data

import           Gdax.Feed.TimeSeries
import qualified Gdax.Types.TimeSeries.Util as TS
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Throttle
import           Gdax.Util.Time

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.IORef
import           Data.Time.Clock
import           Prelude                    hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Time Series" [testSync]

-- Shorten initial period to 1 hour to save loading time for testing
overrideConfig :: Config -> Config
overrideConfig config =
  let initialPeriodOverride = hour
  in config
     {timeSeriesConf = TimeSeriesConf {initialPeriod = initialPeriodOverride}}

testSync :: TestTree
testSync = do
  let syncDelay = 5 :: NominalDiffTime -- 5 seconds
  testCase "should sync with GDAX implementation (takes 5 seconds)" $ do
    tsFeed <-
      runReaderT
        (withReaderT overrideConfig $ newTimeSeriesFeed testGdaxFeed testProduct)
        testConfig
    tsFeedListener <- newFeedListener tsFeed
    initialSeries <- readFeed tsFeedListener
    let (initialStart, initialEnd) = TS.range initialSeries
    countdownRef <- newIORef False
    forkIO $ do
      sleep syncDelay
      writeIORef countdownRef True
    let loop = do
          series <- readFeed tsFeedListener
          timeIsUp <- readIORef countdownRef
          if timeIsUp
            then do
              let (start, end) = TS.range series
              assertEqual "Start time should be equal" initialStart start
              assertBool
                ("End time: " ++
                 show end ++
                 " should be greater than or equal to original time: " ++
                 show initialEnd)
                (end >= initialEnd)
            else loop
    loop
