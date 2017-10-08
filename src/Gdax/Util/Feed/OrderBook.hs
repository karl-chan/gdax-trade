module Gdax.Util.Feed.OrderBook where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax

import           Control.Monad.Reader
import           Prelude                      hiding (product)


newOrderBookFeed :: GdaxFeed -> Product -> ReaderT Config IO OrderBookFeed
newOrderBookFeed gdaxFeed product = do
    gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
    streamOrderBook product gdaxFeedListener
