{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Master where

import           Gdax.Algo.Executor
import           Gdax.Algo.Optimiser
import           Gdax.Algo.Strategy
import           Gdax.Feed.Bundle
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.Gdax
import           Gdax.Feed.MyAccount
import           Gdax.Feed.OrderBook
import           Gdax.Feed.TimeSeries
import           Gdax.Feed.Trades
import           Gdax.Types.Bundle
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Time

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.HashMap.Strict    as HM
import           Gdax.Util.Logger
import           Prelude                hiding (product)

master :: [Product] -> StartTime -> ReaderT Config IO ()
master products startTime = do
  bundleFeed <- createBundleFeed products startTime
  logDebug "Created bundle feed."
    -- bundle arrives every second (refresh_rate in config)
  liftIO $ do
    logDebug "About to create bundle feed listener."
    bundleFeedListener <- newFeedListener bundleFeed
    forever $ do
      logDebug "About to read bundle feed."
      bundle <- readFeed bundleFeedListener
      logDebug "Received bundle."
      runReaderT (trade products) bundle

createBundleFeed :: [Product] -> StartTime -> ReaderT Config IO BundleFeed
createBundleFeed products startTime = do
  gdaxFeed <- newGdaxFeed products
  logDebug "Created GDAX feed."
  accountFeed <- newAccountFeed
  let createMultiFeeds newSingleFeedFn =
        HM.fromList <$>
        (forM products $ \product -> do
           feed <- newSingleFeedFn gdaxFeed product
           return (product, feed))
  seriesFeeds <- createMultiFeeds $ newTimeSeriesFeed startTime
  bookFeeds <- createMultiFeeds $ newOrderBookFeed
  tradesFeeds <- createMultiFeeds $ newTradesFeed
  logDebug "Created all auxiliary feeds."
  newBundleFeed accountFeed bookFeeds seriesFeeds tradesFeeds

trade :: [Product] -> ReaderT Bundle IO ()
trade [product] = do
  logDebug "In trade."
  proposal <- strategy product
  logDebug $ "Computed proposal: " ++ show proposal
  optimisedProposal <- optimise proposal
  logDebug $ "Optimised proposal: " ++ show optimisedProposal
  execute optimisedProposal
  logDebug "Executed optimised proposal"
trade products =
  error $
  "Only one product is supported at this time, but got: " ++ show products
