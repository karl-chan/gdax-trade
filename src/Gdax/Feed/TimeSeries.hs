module Gdax.Feed.TimeSeries
  ( module Gdax.Feed.TimeSeries.Types
  , module Gdax.Feed.TimeSeries
  ) where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.TimeSeries.Internal
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Time

import           Control.Monad.Reader
import           Prelude                       hiding (product)

newTimeSeriesFeed ::
     StartTime -> GdaxFeed -> Product -> ReaderT Config IO TimeSeriesFeed
newTimeSeriesFeed startTime gdaxFeed product = do
  gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
  streamTimeSeries startTime product gdaxFeedListener
