module Gdax.Feed.OrderBook
    ( module Gdax.Feed.OrderBook.Types
    , module Gdax.Feed.OrderBook
    ) where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.OrderBook.Internal
import           Gdax.Feed.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Gdax.Util.Logger
import           Control.Monad.Reader

newOrderBookFeed :: GdaxFeed -> Product -> ReaderT Config IO OrderBookFeed
newOrderBookFeed gdaxFeed product = do
    gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
    streamOrderBook product gdaxFeedListener
