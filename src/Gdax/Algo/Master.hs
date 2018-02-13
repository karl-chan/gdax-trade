{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Master where

import           Gdax.Algo.Executor
import           Gdax.Algo.Optimiser
import           Gdax.Algo.Strategy
import           Gdax.Feed.Bundle
import           Gdax.Feed.Gdax
import           Gdax.Types.Bundle
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad
import           Control.Monad.Reader
import           Gdax.Util.Logger
import           Prelude              hiding (product)

master :: [Product] -> ReaderT Config IO ()
master products = do
  gdaxFeed <- newGdaxFeed products
  bundleFeed <- newBundleFeed products gdaxFeed
  logDebug "Created bundle feed."
    -- bundle arrives every second (refresh_rate in config)
  bundleFeedListener <- liftIO $ newFeedListener bundleFeed
  logDebug "Created bundle feed listener."
  liftIO $
    forever $ do
      bundle <- readFeed bundleFeedListener
      runReaderT (trade products) bundle

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
