module Gdax.Data.OrderBook where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types      (ExchangeConf)

import           BroadcastChan.Throw          (newBroadcastChan)
import           Control.Concurrent           (forkIO)
import           Control.Monad.Reader         (ReaderT, liftIO, runReaderT)
import qualified Data.HashMap                 as Map
import           Data.List                    (maximumBy, minimumBy)
import           Data.Ord                     (comparing)

liveOrderBookFeed :: Product -> ProductFeed -> ReaderT Config IO OrderBookFeed
liveOrderBookFeed product productFeed = do
    productFeedListener <- liftIO $ newFeedListener productFeed
    processOrderBook product productFeedListener

-- Util methods below --
ask :: OrderBook -> OrderBookItem
ask = minimumBy (comparing price) . Map.elems . bookAsks

bid :: OrderBook -> OrderBookItem
bid = maximumBy (comparing price) . Map.elems . bookBids
