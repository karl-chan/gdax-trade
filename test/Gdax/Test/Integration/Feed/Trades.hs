module Gdax.Test.Integration.Feed.Trades where

import           Gdax.Test.Data

import           Gdax.Feed.Trades
import qualified Gdax.Types.Trades.Util as Trades
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Time

import           Control.Monad.Reader
import           Data.Time.Clock
import           Prelude                hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = do
  testCase "Trades" test

test :: Assertion
test = do
  now <- getCurrentTime
  tradesFeed <- runReaderT (newTradesFeed testGdaxFeed testProduct) testConfig
  tradesFeedListener <- newFeedListener tradesFeed
  initialTrades <- readFeed tradesFeedListener
  let (initialStart, initialEnd) = Trades.range initialTrades
      loop = do
        trades <- readFeed tradesFeedListener
        if trades /= initialTrades
          then do
            let (start, end) = Trades.range trades
                margin = 5 * minute
                window = rollingWindow . tradesConf $ testConfig
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
            assertBool
              ("Start time: " ++
               show start ++
               " should be greater than or equal to initial start time: " ++
               show initialStart)
              (start >= initialStart)
            assertBool
              ("End time: " ++
               show end ++
               " should be greater than initial end time: " ++ show initialEnd)
              (end > initialEnd)
          else loop
  loop
