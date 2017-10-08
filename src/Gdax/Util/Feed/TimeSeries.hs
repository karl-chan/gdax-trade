module Gdax.Util.Feed.TimeSeries where

import           Gdax.Data.TimeSeries.Internal
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax

import           Control.Monad.Reader          (ReaderT, liftIO)
import           Prelude                       hiding (product)

newTimeSeriesFeed :: GdaxFeed -> StartTime -> Product -> ReaderT Config IO TimeSeriesFeed
newTimeSeriesFeed gdaxFeed startTime product = do
    gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
    streamTimeSeries startTime product gdaxFeedListener
