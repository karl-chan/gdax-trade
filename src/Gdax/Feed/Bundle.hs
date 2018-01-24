module Gdax.Feed.Bundle where

import           Gdax.Feed.Bundle.Internal
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.MyAccount.Types
import           Gdax.Feed.OrderBook.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader
import           Data.HashMap.Strict        (HashMap)

newBundleFeed ::
     MyAccountFeed
  -> HashMap Product OrderBookFeed
  -> HashMap Product TimeSeriesFeed
  -> HashMap Product TradesFeed
  -> ReaderT Config IO BundleFeed
newBundleFeed accountFeed bookFeeds seriesFeeds tradesFeeds = do
  accountFeedListener <- liftIO $ newFeedListener accountFeed
  bookFeedListeners <- liftIO $ mapM newFeedListener bookFeeds
  seriesFeedListeners <- liftIO $ mapM newFeedListener seriesFeeds
  tradesFeedListeners <- liftIO $ mapM newFeedListener tradesFeeds
  streamBundle
    accountFeedListener
    bookFeedListeners
    seriesFeedListeners
    tradesFeedListeners
