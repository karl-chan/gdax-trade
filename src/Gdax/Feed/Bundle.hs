module Gdax.Feed.Bundle where

import           Gdax.Feed.Bundle.Internal
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.Gdax
import           Gdax.Feed.MyAccount
import           Gdax.Feed.OrderBook
import           Gdax.Feed.TimeSeries
import           Gdax.Feed.Trades
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Logger

import           Control.Monad.Reader
import qualified Data.HashMap.Strict       as HM
import           Prelude                   hiding (product)

newBundleFeed :: [Product] -> GdaxFeed -> ReaderT Config IO BundleFeed
newBundleFeed products gdaxFeed = do
  accountFeed <- newAccountFeed
  let createMultiFeeds newSingleFeedFn =
        HM.fromList <$>
        (forM products $ \product -> do
           feed <- newSingleFeedFn product gdaxFeed
           return (product, feed))
  seriesFeeds <- createMultiFeeds newTimeSeriesFeed
  bookFeeds <- createMultiFeeds newOrderBookFeed
  tradesFeeds <- createMultiFeeds newTradesFeed
  logDebug "Created all auxiliary feeds."
  accountFeedListener <- liftIO $ newFeedListener accountFeed
  bookFeedListeners <- liftIO $ mapM newFeedListener bookFeeds
  seriesFeedListeners <- liftIO $ mapM newFeedListener seriesFeeds
  tradesFeedListeners <- liftIO $ mapM newFeedListener tradesFeeds
  logDebug "Created all auxiliary feed listeners."
  streamBundle
    accountFeedListener
    bookFeedListeners
    seriesFeedListeners
    tradesFeedListeners
