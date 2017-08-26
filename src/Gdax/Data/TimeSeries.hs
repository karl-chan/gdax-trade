module Gdax.Data.TimeSeries where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.TimeSeries.Internal
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types       (ExchangeConf)
import           Coinbase.Exchange.Types.Core  (ProductId)

import           Control.Concurrent            (forkIO)
import           Control.Monad.Reader          (ReaderT, liftIO)
import           Data.Time.Clock               (UTCTime)

liveTSFeed :: StartTime -> Product -> ProductFeed -> ReaderT Config IO TimeSeriesFeed
liveTSFeed startTime product productFeed = do
    productFeedListener <- liftIO $ newFeedListener productFeed
    processSeries startTime product productFeedListener
