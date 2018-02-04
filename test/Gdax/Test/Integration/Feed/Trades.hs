module Gdax.Test.Integration.Feed.Trades where

import           Gdax.Test.Data

import           Gdax.Feed.Gdax
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
  testGroup "Trades" [testRange]

testRange :: TestTree
testRange =
  testCase "should span across rolling window" $ do
    now <- getCurrentTime
    gdaxFeed <- runReaderT (newGdaxFeed [testProduct]) testConfig
    tradesFeed <- runReaderT (newTradesFeed gdaxFeed testProduct) testConfig
    initialTrades <- readFeed tradesFeed
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
