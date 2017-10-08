module Gdax.Algo.Master where

import           Gdax.Account.MyAccount
import           Gdax.Algo.Cost
import           Gdax.Algo.Executor
import           Gdax.Algo.Strategy.Spread
import           Gdax.Algo.Types
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Bundle
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Bundle
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Feed.MyAccount
import           Gdax.Util.Feed.OrderBook
import           Gdax.Util.Feed.TimeSeries

import           Control.Monad
import           Control.Monad.Reader
import           Data.List                  hiding (product)
import           Data.Ord
import           Prelude                    hiding (product)

allStrategies :: [Strategy]
allStrategies = [spread]

master :: [Product] -> StartTime -> ReaderT Config IO ()
master products startTime = do
    gdaxFeed <- newGdaxFeed products
    bookFeeds <- mapM (newOrderBookFeed gdaxFeed) products
    seriesFeeds <- mapM (newTimeSeriesFeed gdaxFeed startTime) products
    accountFeed <- newAccountFeed gdaxFeed
    bundleFeed <- newBundleFeed bookFeeds seriesFeeds accountFeed
    liftIO . forever $ do
        bundle <- readFeed bundleFeed
        runReaderT (trade products) bundle

trade :: [Product] -> ReaderT Bundle IO ()
trade products =
    forever $ do
        proposals <- sequence [explore strategy product | strategy <- allStrategies, product <- products]
        let (actions, returns) = maximumBy comparingReturns proposals
        mapM_ execute actions

explore :: Strategy -> Product -> ReaderT Bundle IO Proposal
explore strategy product = do
    (actions, expectedReturn) <- strategy product
    totalCost <- msum [calculateCost action | action <- actions]
    return (actions, expectedReturn - totalCost)

comparingReturns :: Proposal -> Proposal -> Ordering
comparingReturns = comparing snd
