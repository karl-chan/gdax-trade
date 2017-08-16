module Gdax.Data.TimeSeries where

import Gdax.Util.Feed
import Gdax.Data.Product
import Gdax.Data.OrderBook.Types
import Gdax.Data.TimeSeries.Types
import Gdax.Data.TimeSeries.Internal
import Gdax.Util.Config

import           Coinbase.Exchange.Types      (ExchangeConf)
import           Coinbase.Exchange.Types.Core (ProductId)
import Control.Concurrent (forkIO)

import           Data.Time.Clock (UTCTime)


liveTSFeed :: StartTime -> Granularity -> ProductId -> ProductFeed -> Config -> IO TimeSeriesFeed
liveTSFeed startTime granularity productId productFeed config = do
    tsFeed <- newFeed
    forkIO $ processSeries startTime granularity productId productFeed tsFeed config
    return tsFeed