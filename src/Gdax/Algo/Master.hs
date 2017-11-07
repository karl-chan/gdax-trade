{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Master where

import           Gdax.Algo.Cost
import           Gdax.Algo.Executor
import           Gdax.Algo.Strategy.Spread
import           Gdax.Algo.Types
import           Gdax.Feed.Bundle
import           Gdax.Feed.Gdax
import           Gdax.Feed.MyAccount
import           Gdax.Feed.OrderBook
import           Gdax.Feed.TimeSeries
import           Gdax.Types.Bundle
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad
import           Control.Monad.Reader
import           Data.List
import           Data.Ord
import           Gdax.Util.Logger

allStrategies :: [Strategy]
allStrategies = [spread]

master :: [Product] -> StartTime -> ReaderT Config IO ()
master products startTime = do
    gdaxFeed <- newGdaxFeed products
    logDebug "Created GDAX feed."
    seriesFeeds <- mapM (newTimeSeriesFeed gdaxFeed startTime) products
    bookFeeds <- mapM (newOrderBookFeed gdaxFeed) products
    accountFeed <- newAccountFeed gdaxFeed
    logDebug "Created all auxiliary feeds."
    bundleFeed <- newBundleFeed bookFeeds seriesFeeds accountFeed
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

trade :: [Product] -> ReaderT Bundle IO ()
trade products = do
    logDebug "In trade."
    proposals <- sequence [propose strategy product | strategy <- allStrategies, product <- products]
    logDebug "Computed proposals."
    let bestProposal = maximumBy (comparing netProfit) proposals
    logDebug $ "Selected best proposal: " ++ show bestProposal
    execute bestProposal
    logDebug "Executed best proposal."

propose :: Strategy -> Product -> ReaderT Bundle IO Proposal
propose strategy product = do
    logDebug "Inside propose."
    StrategyProposal {..} <- strategy product
    logDebug $ "Calculated strategy proposal."
    cost <- msum [calculateCost action | action <- strategyActions]
    logDebug "Calculated cost"
    let fullProposal =
            Proposal
            { description = strategyName
            , actions = strategyActions
            , profit = strategyEstimatedProfit
            , cost = cost
            , netProfit = strategyEstimatedProfit - cost
            }
    logDebug $ "Full proposal: " ++ show fullProposal
    return fullProposal
