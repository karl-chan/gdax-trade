module Gdax.Data.TimeSeries where

import           Gdax.Data.TimeSeries.Internal
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader          (ReaderT, liftIO)

liveTSFeed :: StartTime -> Product -> ProductFeed -> ReaderT Config IO TimeSeriesFeed
liveTSFeed startTime product productFeed = do
    productFeedListener <- liftIO $ newFeedListener productFeed
    processSeries startTime product productFeedListener
