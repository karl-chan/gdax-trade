module Gdax.Test.Data.OrderBook where

import           Gdax.Test.Data.Product

import           Gdax.Types.OrderBook

import           Coinbase.Exchange.Types.Core

import qualified Data.HashMap.Strict          as HM
import           Data.Maybe
import qualified Data.UUID                    as UUID

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
  [ OrderBookItem {price = 99, size = 0.1, orderId = testOrderId1}
  , OrderBookItem {price = 98, size = 0.2, orderId = testOrderId2}
  , OrderBookItem {price = 97, size = 0.3, orderId = testOrderId3}
  ]

testBookAsks :: [OrderBookItem]
testBookAsks =
  [ OrderBookItem {price = 101, size = 0.1, orderId = testOrderId4}
  , OrderBookItem {price = 102, size = 0.2, orderId = testOrderId5}
  , OrderBookItem {price = 103, size = 0.3, orderId = testOrderId6}
  ]

testOrderId1 :: OrderId
testOrderId1 = toOrderId "00000000-0000-0000-0000-000000000001"

testOrderId2 :: OrderId
testOrderId2 = toOrderId "00000000-0000-0000-0000-000000000002"

testOrderId3 :: OrderId
testOrderId3 = toOrderId "00000000-0000-0000-0000-000000000003"

testOrderId4 :: OrderId
testOrderId4 = toOrderId "00000000-0000-0000-0000-000000000004"

testOrderId5 :: OrderId
testOrderId5 = toOrderId "00000000-0000-0000-0000-000000000005"

testOrderId6 :: OrderId
testOrderId6 = toOrderId "00000000-0000-0000-0000-000000000006"

toOrderId :: String -> OrderId
toOrderId = OrderId . fromJust . UUID.fromString
