{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Types.OrderBook where

import           Coinbase.Exchange.Types.Core

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.HashMap.Strict          (HashMap)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)

import           Gdax.Types.Product

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


data OrderBookSummary = OrderBookSummary
    { bestBid  :: Price
    , bestAsk  :: Price
    , midPrice :: Price
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)
