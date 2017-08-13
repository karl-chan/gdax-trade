module Gdax.Data.TimeSeries where

import Gdax.Util.Feed
import Gdax.Data.Product
import Gdax.Data.OrderBook.Types
import Gdax.Data.TimeSeries.Types
import Gdax.Data.TimeSeries.Internal

import           Coinbase.Exchange.Types      (ExchangeConf)
import           Coinbase.Exchange.Types.Core (ProductId)
import Control.Concurrent (forkIO)

import           Data.Time.Clock (UTCTime)


liveTSFeed :: StartTime -> Granularity -> ProductId -> ExchangeConf -> ProductFeed -> IO TimeSeriesFeed
liveTSFeed startTime granularity productId conf productFeed = do
    tsFeed <- newFeed
    forkIO $ processSeries startTime granularity productId conf productFeed tsFeed
    return tsFeed