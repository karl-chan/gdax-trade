{-# LANGUAGE RecordWildCards #-}

module Gdax.Test.Unit.Algo.Cost where

import           Gdax.Test.Data
import           Gdax.Test.Util.Math

import           Gdax.Algo.Action
import           Gdax.Algo.Cost
import           Gdax.Algo.Types
import           Gdax.Types.Amount

import           Coinbase.Exchange.Types.Core (Price, Side (..), Size)

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = do
  testGroup "Cost" [testMarketSpreadCost, testActualSize, testActualPrice]

-- | Takes how many coins to sell fiat at market rate, vice versa
-- | Mid price: 1 coin = 100 fiat
bidEquivalents :: [(Size, Price)]
bidEquivalents =
  [(0.05, 4.95), (0.1, 9.9), (0.2, 19.7), (0.3, 29.5), (0.4, 39.2), (0.6, 58.6)]

-- | Takes how many coins to buy fiat at market rate, vice versa
askEquivalents :: [(Size, Price)]
askEquivalents =
  [ (0.05, 5.05)
  , (0.1, 10.1)
  , (0.2, 20.3)
  , (0.3, 30.5)
  , (0.4, 40.8)
  , (0.6, 61.4)
  ]

bidCosts :: [(Size, Cost)]
bidCosts =
  [ (0.05, (5 - 4.95) / 5)
  , (0.1, (10 - 9.9) / 10)
  , (0.2, (20 - 19.7) / 20)
  , (0.3, (30 - 29.5) / 30)
  , (0.4, (40 - 39.2) / 40)
  , (0.6, (60 - 58.6) / 60)
  ]

askCosts :: [(Size, Cost)]
askCosts =
  [ (0.05, (5.05 - 5) / 5)
  , (0.1, (10.1 - 10) / 10)
  , (0.2, (20.3 - 20) / 20)
  , (0.3, (30.5 - 30) / 30)
  , (0.4, (40.8 - 40) / 40)
  , (0.6, (61.4 - 60) / 60)
  ]

-- Test market spread cost
testMarketSpreadCost :: TestTree
testMarketSpreadCost =
  testGroup
    "Market spread cost"
    [ testCase "Buy" $ testMarketSpreadCostForSide Buy askCosts
    , testCase "Sell" $ testMarketSpreadCostForSide Sell bidCosts
    ]
  where
    testMarketSpreadCostForSide :: Side -> [(Size, Cost)] -> Assertion
    testMarketSpreadCostForSide side expectations = do
      forM_ expectations $ \(size, cost) -> do
        let action =
              Market
              {side = side, product = testProduct, amount = AmountSize size}
        assertRoughlyEqual
          ("Market spread cost should be %f instead of %f for " ++
           show (realToFrac size :: Double) ++ " coins")
          cost
          (marketSpreadCost action testOrderBook)

-- Test actual size
testActualSize :: TestTree
testActualSize =
  testGroup
    "Actual size"
    [ testCase "Buy" $ testActualSizeForSide Buy askEquivalents
    , testCase "Sell" $ testActualSizeForSide Sell bidEquivalents
    ]
  where
    testActualSizeForSide :: Side -> [(Size, Price)] -> Assertion
    testActualSizeForSide side expectations = do
      let bookItems =
            case side of
              Buy  -> testBookAsks
              Sell -> testBookBids
      forM_ expectations $ \(coin, fiat) ->
        assertRoughlyEqual
          ("%f coins should be required instead of %f to " ++
           show side ++
           " " ++
           show (realToFrac fiat :: Double) ++ " fiat from order book items")
          coin
          (actualSize bookItems fiat)

-- Test actual price
testActualPrice :: TestTree
testActualPrice =
  testGroup
    "Actual price"
    [ testCase "Buy" $ testActualPriceForSide Buy askEquivalents
    , testCase "Sell" $ testActualPriceForSide Sell bidEquivalents
    ]
  where
    testActualPriceForSide :: Side -> [(Size, Price)] -> Assertion
    testActualPriceForSide side expectations = do
      let bookItems =
            case side of
              Buy  -> testBookAsks
              Sell -> testBookBids
      forM_ expectations $ \(coin, fiat) ->
        assertRoughlyEqual
          ("%f fiat should be required instead of %f to " ++
           show side ++
           " " ++
           show (realToFrac coin :: Double) ++ " coins from order book items")
          fiat
          (actualPrice bookItems coin)
