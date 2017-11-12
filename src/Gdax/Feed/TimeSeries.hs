module Gdax.Feed.TimeSeries
  ( module Gdax.Feed.TimeSeries.Types
  , module Gdax.Feed.TimeSeries
  ) where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.TimeSeries.Internal
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries         hiding (product)
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader
import           Prelude                       hiding (product)

newTimeSeriesFeed ::
     GdaxFeed -> StartTime -> Product -> ReaderT Config IO TimeSeriesFeed
newTimeSeriesFeed gdaxFeed startTime product = do
  gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
  streamTimeSeries startTime product gdaxFeedListener
