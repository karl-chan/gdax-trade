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
  testGroup "Trades" [test]

test :: TestTree
test =
  testCase "should span across rolling window" $ do
    now <- getCurrentTime
    tradesFeed <- runReaderT (newTradesFeed testGdaxFeed testProduct) testConfig
    tradesFeedListener <- newFeedListener tradesFeed
    initialTrades <- readFeed tradesFeedListener
    let (s, e) = Trades.range initialTrades
        margin = 10 * minute
        window = rollingWindow . tradesConf $ testConfig
        startOfWindow = addUTCTime (-window) now
    assertBool
      ("Start time: " ++
       show s ++ " should be close to start of window: " ++ show startOfWindow)
      (diffUTCTime s startOfWindow < margin)
    assertBool
      ("End time: " ++ show e ++ " should be close to now: " ++ show now)
      (diffUTCTime now e < margin)
