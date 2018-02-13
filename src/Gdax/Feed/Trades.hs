module Gdax.Feed.Trades
  ( module Gdax.Feed.Trades.Types
  , module Gdax.Feed.Trades
  ) where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.Trades.Internal
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader
import           Prelude                   hiding (product)

newTradesFeed :: Product -> GdaxFeed -> ReaderT Config IO TradesFeed
newTradesFeed product gdaxFeed = do
  gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
  streamTrades product gdaxFeedListener
