{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Util where

import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Util.Config

import           Coinbase.Exchange.MarketData       (getOrderBook)
import           Coinbase.Exchange.Types            (execExchangeT)
import           Coinbase.Exchange.Types.Core       (OrderId)
import           Coinbase.Exchange.Types.MarketData (Book (..),
                                                     BookItem (BookItem))

import           Control.Monad.Reader
import qualified Data.HashMap.Strict                as Map

restOrderBook :: Product -> ReaderT Config IO OrderBook
restOrderBook product = do
    conf <- reader exchangeConf
    rawBook <- execExchangeT conf $ getOrderBook (toId product)
    return $ fromRawOrderBook rawBook

fromRawOrderBook :: Book OrderId -> OrderBook
fromRawOrderBook Book {..} =
    OrderBook {bookSequence = bookSequence, bookBids = fromRawBookItems bookBids, bookAsks = fromRawBookItems bookAsks}
  where
    fromRawBookItems rawBookItems = Map.fromList $ map toKeyValue rawBookItems
    toKeyValue (BookItem price size orderId) = (orderId, OrderBookItem price size orderId)
