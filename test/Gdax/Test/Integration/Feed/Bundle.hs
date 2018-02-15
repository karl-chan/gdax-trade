{-# LANGUAGE RecordWildCards #-}

module Gdax.Test.Integration.Feed.Bundle where

import           Gdax.Test.Data
import           Gdax.Test.Util.Math

import           Gdax.Feed.Bundle
import           Gdax.Types.Bundle
import           Gdax.Util.Feed
import           Gdax.Util.Time

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as HM
import           Data.Time.Clock
import           Prelude              hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Bundle" $ test

test :: Assertion
test
  -- let compareAgainAfter = 5 * second
 = do
  bundleFeed <- runReaderT (newBundleFeed [testProduct] testGdaxFeed) testConfig
  bundleFeedListener <- newFeedListener bundleFeed
  initialBundle <- readFeed bundleFeedListener
  let [(bookProduct, _)] = HM.toList $ books initialBundle
      [(seriesProduct, _)] = HM.toList $ multiSeries initialBundle
      [(tradesProduct, _)] = HM.toList $ multiTrades initialBundle
  -- Check that feed product and config matches
  forM_
    [ ("book", bookProduct)
    , ("series", seriesProduct)
    , ("trades", tradesProduct)
    ] $ \(name, product) -> do
    assertEqual
      (name ++ " product should be equal to test product")
      testProduct
      product
  assertEqual
    "Config should be equal to test config"
    testConfig
    (config initialBundle)
  -- Check that feed comes every second
  let feedIntervalTolerance = 0.1 * second
  forM_ [1 .. 5 :: Int] $ \_ -> do
    startTimer <- getCurrentTime
    _ <- readFeed bundleFeedListener
    endTimer <- getCurrentTime
    assertRoughlyEqualWithTolerance
      (realToFrac feedIntervalTolerance)
      "Expected %f instead of %f for bundle feed gap"
      second
      (diffUTCTime endTimer startTimer)
