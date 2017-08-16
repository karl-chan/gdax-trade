module Gdax.Data.OrderBook where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types      (ExchangeConf)
import           Coinbase.Exchange.Types.Core (ProductId)

import Control.Monad.Reader (ReaderT, runReaderT, liftIO)
import           BroadcastChan.Throw          (newBroadcastChan)
import           Control.Concurrent           (forkIO)
import qualified Data.HashMap                 as Map
import           Data.List                    (maximumBy, minimumBy)
import           Data.Ord                     (comparing)

liveOrderBookFeed :: ProductId -> ProductFeed ->  ReaderT Config IO OrderBookFeed
liveOrderBookFeed productId productFeed = do
    productFeedListener<- liftIO $ newFeedListener productFeed
    processOrderBook productId productFeedListener

ask :: OrderBook -> OrderBookItem
ask = minimumBy (comparing price) . Map.elems . bookAsks

bid :: OrderBook -> OrderBookItem
bid = maximumBy (comparing price) . Map.elems . bookBids
