module Gdax.Feed.Bundle where

import           Gdax.Feed.Bundle.Internal
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.MyAccount.Types
import           Gdax.Feed.OrderBook.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader

newBundleFeed ::
     [OrderBookFeed]
  -> [TimeSeriesFeed]
  -> MyAccountFeed
  -> ReaderT Config IO BundleFeed
newBundleFeed bookFeeds seriesFeeds accountFeed = do
  bookFeedListeners <- liftIO $ mapM newFeedListener bookFeeds
  seriesFeedListeners <- liftIO $ mapM newFeedListener seriesFeeds
  accountFeedListener <- liftIO $ newFeedListener accountFeed
  streamBundle bookFeedListeners seriesFeedListeners accountFeedListener
