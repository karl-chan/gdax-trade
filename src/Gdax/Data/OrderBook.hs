{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad.Reader         (liftIO, ReaderT)
import qualified Data.HashMap.Strict          as Map
import           Data.List

liveOrderBookFeed :: Product -> ProductFeed -> ReaderT Config IO OrderBookFeed
liveOrderBookFeed product productFeed = do
    productFeedListener <- liftIO $ newFeedListener productFeed
    processOrderBook product productFeedListener

-- Util methods below --
-- Asks sorted in ascending order of price
sortedAsks :: OrderBook -> [OrderBookItem]
sortedAsks OrderBook {..} = sortOn price $ Map.elems bookAsks

-- Bids sorted in descending order of price
sortedBids :: OrderBook -> [OrderBookItem]
sortedBids OrderBook {..} = sortOn (negate . price) $ Map.elems bookBids
