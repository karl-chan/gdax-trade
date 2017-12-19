module Gdax.Feed.Trades
  ( module Gdax.Feed.TimeSeries.Types
  , module Gdax.Feed.TimeSeries
  ) where

import Gdax.Feed.Gdax.Types
import Gdax.Feed.Trades.Internal
import Gdax.Feed.Trades.Types
import Gdax.Types.Product
import Gdax.Types.Trade
import Gdax.Util.Config
import Gdax.Util.Feed

import Control.Monad.Reader
import Prelude hiding (product)

newTradesFeed :: GdaxFeed -> Product -> ReaderT Config IO TradesFeed
newTradesFeed gdaxFeed product = do
  gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
  streamTrades product gdaxFeedListener
