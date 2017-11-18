{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Cost.Test where

import           Gdax.Algo.Cost
import           Gdax.Algo.Cost.Test.Internal
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook.Test
import           Gdax.Util.Math.Test

import           Coinbase.Exchange.Types.Core

import           Control.Monad
import           Control.Monad.Reader
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: ReaderT Bundle IO TestTree
tests = do
  liftM2 testGroup (return "CostCalculator") $
    sequence [testMarketSpreadCost, testActualSize, testActualPrice]

-- | Takes how many coins to sell fiat at market rate, vice versa
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

testMarketSpreadCost :: ReaderT Bundle IO TestTree
testMarketSpreadCost = do
  liftM2 testGroup (return "marketSpreadCost") $
    sequence
      [ testMarketActionSpreadCost
      , liftM2 testCase (return "for limit action") testLimitActionSpreadCost
      , liftM2 testCase (return "for stop action") testStopActionSpreadCost
      ]

testMarketActionSpreadCost :: ReaderT Bundle IO TestTree
testMarketActionSpreadCost = do
  liftM2 testGroup (return "for market action") $
    sequence
      [ liftM2
          testCase
          (return "for buy" $ testMarketActionSpreadCostForSide Buy askCosts)
      , liftM2
          testCase
          (return "for sell" $ testMarketActionSpreadCostForSide Sell buyCosts)
      ]

testActualSize :: ReaderT Bundle IO TestTree
testActualSize =
  liftM2 testGroup (return "actualSize") $
  sequence
    [ liftM2 testCase (return "for buy") $
      testActualSizeForSide Buy askEquivalents
    , liftM2 testCase (return "for sell") $
      testActualSizeForSide Sell bidEquivalents
    ]

testActualPrice :: ReaderT Bundle IO TestTree
testActualPrice =
  liftM2 testGroup (return "actualPrice") $
  sequence
    [ liftM2 testCase (return "for buy") $
      testActualPriceForSide Buy askEquivalents
    , liftM2 testCase (return "for sell") $
      testActualPriceForSide Sell bidEquivalents
    ]
