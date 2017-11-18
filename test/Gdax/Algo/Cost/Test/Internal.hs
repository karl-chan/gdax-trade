module Gdax.Algo.Cost.Test.Internal where

import           Gdax.Algo.Action
import           Gdax.Algo.Cost
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook.Test
import           Gdax.Types.Product.Test
import           Gdax.Util.Math.Test

import           Coinbase.Exchange.Types.Core (Price, Side (Buy, Sell), Size)

import           Control.Monad
import           Control.Monad.Reader
import           Prelude                      hiding (product)
import           Test.Tasty.HUnit

testMarketActionSpreadCostForSide ::
     Side -> [(Size, Cost)] -> ReaderT Bundle IO Assertion
testMarketActionSpreadCostForSide side expectations = do
  return $
    forM_ expectations $ \(size, cost) -> do
      let action =
            Market
            {side = side, product = testProduct, amount = AmountSize size}
      assertRoughlyEqual
        ("Market spread cost should be " ++
         show cost ++ " for " ++ show (realToFrac size :: Double) ++ " coins")
        cost
        (marketSpreadCost action testOrderBook)

testActualPriceForSide :: Side -> [(Size, Price)] -> ReaderT Bundle IO Assertion
testActualPriceForSide side expectations = do
  let bookItems =
        case side of
          Buy  -> testBookAsks
          Sell -> testBookBids
  return $
    forM_ expectations $ \(coin, fiat) ->
      assertRoughlyEqual
        (show (realToFrac fiat :: Double) ++
         "fiat should be required to " ++
         show side ++
         " " ++
         show (realToFrac coin :: Double) ++ " coins from order book items")
        fiat
        (actualPrice bookItems coin)

testActualSizeForSide :: Side -> [(Size, Price)] -> ReaderT Bundle IO Assertion
testActualSizeForSide side expectations = do
  let bookItems =
        case side of
          Buy  -> testBookAsks
          Sell -> testBookBids
  return $
    forM_ expectations $ \(coin, fiat) ->
      assertRoughlyEqual
        (show (realToFrac coin :: Double) ++
         "coins should be required to " ++
         show side ++
         " " ++
         show (realToFrac fiat :: Double) ++ " fiat from order book items")
        coin
        (actualSize bookItems fiat)
