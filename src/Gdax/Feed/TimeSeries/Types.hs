module Gdax.Feed.TimeSeries.Types where

import           Gdax.Types.TimeSeries
import           Gdax.Util.Feed

type TimeSeriesFeed = Feed TimeSeries

type TimeSeriesFeedListener = FeedListener TimeSeries
