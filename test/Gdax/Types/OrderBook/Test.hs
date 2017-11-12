module Gdax.Types.OrderBook.Test where

import           Gdax.Types.OrderBook
import           Gdax.Types.Product.Test

import           Coinbase.Exchange.Types.Core

import qualified Data.HashMap.Strict          as HM
import           Data.Maybe
import           Data.UUID

testOrderBook :: OrderBook
testOrderBook =
  OrderBook
  { bookSequence = 1
  , bookBids = HM.fromList $ map (\b -> (orderId b, b)) testBookBids
  , bookAsks = HM.fromList $ map (\b -> (orderId b, b)) testBookAsks
  , bookProduct = testProduct
  }

testBookBids :: [OrderBookItem]
testBookBids =
  [ OrderBookItem
    { price = 99
    , size = 0.1
    , orderId = toOrderId "00000000-0000-0000-0000-000000000001"
    }
  , OrderBookItem
    { price = 98
    , size = 0.2
    , orderId = toOrderId "00000000-0000-0000-0000-000000000002"
    }
  , OrderBookItem
    { price = 97
    , size = 0.3
    , orderId = toOrderId "00000000-0000-0000-0000-000000000003"
    }
  ]

testBookAsks :: [OrderBookItem]
testBookAsks =
  [ OrderBookItem
    { price = 101
    , size = 0.1
    , orderId = toOrderId "00000000-0000-0000-0000-000000000004"
    }
  , OrderBookItem
    { price = 102
    , size = 0.2
    , orderId = toOrderId "00000000-0000-0000-0000-000000000005"
    }
  , OrderBookItem
    { price = 103
    , size = 0.3
    , orderId = toOrderId "00000000-0000-0000-0000-000000000006"
    }
  ]

toOrderId :: String -> OrderId
toOrderId str = OrderId . fromJust . fromString $ str
