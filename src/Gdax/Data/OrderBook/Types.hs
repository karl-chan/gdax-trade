{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Data.OrderBook.Types where

import           Coinbase.Exchange.Types.Core (OrderId, Price, Sequence, Size)

import           BroadcastChan.Throw          (BroadcastChan, In)
import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.HashMap                 (Map)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)

data OrderBook = OrderBook
    { bookSequence :: Sequence
    , bookBids     :: OrderBookItems
    , bookAsks     :: OrderBookItems
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)

data OrderBookItem = OrderBookItem
    { price   :: Price
    , size    :: Size
    , orderId :: OrderId
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)

type OrderBookItems = Map OrderId OrderBookItem

type OrderBookBroadcastChan = BroadcastChan In OrderBook
