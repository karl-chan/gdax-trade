module Gdax.Data.OrderBook where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types      (ExchangeConf)
import           Coinbase.Exchange.Types.Core (ProductId)

import           BroadcastChan.Throw          (newBroadcastChan)
import           Control.Concurrent           (forkIO)
import qualified Data.HashMap                 as Map
import           Data.List                    (maximumBy, minimumBy)
import           Data.Ord                     (comparing)

livecastOrderBook :: ProductId -> ExchangeConf -> Feed -> IO OrderBookBroadcastChan
livecastOrderBook productId conf feed = do
    orderBookBroadcastChan <- newBroadcastChan
    feedListener <- waitUntilFeed =<< newFeedListener feed
    forkIO $ processOrderBook productId conf feedListener orderBookBroadcastChan
    return orderBookBroadcastChan

ask :: OrderBook -> OrderBookItem
ask = (minimumBy $ comparing price) . Map.elems . bookAsks

bid :: OrderBook -> OrderBookItem
bid = (maximumBy $ comparing price) . Map.elems . bookBids
