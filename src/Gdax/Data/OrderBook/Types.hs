{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Data.OrderBook.Types where

import           Gdax.Types.Product
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types.Core (OrderId, Price, Sequence, Size)

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.HashMap.Strict          (HashMap)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (product)

data OrderBook = OrderBook
    { bookSequence :: Sequence
    , bookBids     :: OrderBookItems
    , bookAsks     :: OrderBookItems
    , bookProduct  :: Product
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)

data OrderBookItem = OrderBookItem
    { price   :: Price
    , size    :: Size
    , orderId :: OrderId
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)

type OrderBookItems = HashMap OrderId OrderBookItem

type OrderBookFeed = Feed OrderBook

type OrderBookFeedListener = FeedListener OrderBook