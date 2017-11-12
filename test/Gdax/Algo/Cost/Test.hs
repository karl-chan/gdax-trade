{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Cost.Test where

import           Gdax.Algo.Cost
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
    sequence
      [ liftM2 testCase (return "actualSize") testActualSize
      , liftM2 testCase (return "actualPrice") testActualPrice
      ]

bidExpectations :: [(Size, Price)]
bidExpectations =
  [(0.05, 4.95), (0.1, 9.9), (0.2, 19.7), (0.3, 29.5), (0.4, 39.2), (0.6, 58.6)]

askExpectations :: [(Size, Price)]
askExpectations =
  [ (0.05, 5.05)
  , (0.1, 10.1)
  , (0.2, 20.3)
  , (0.3, 30.5)
  , (0.4, 40.8)
  , (0.6, 61.4)
  ]

testActualSize :: ReaderT Bundle IO Assertion
testActualSize = do
  testActualSize' Buy bidExpectations testBookBids
  testActualSize' Sell askExpectations testBookAsks
  where
    testActualSize' side expectations bookItems =
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

testActualPrice :: ReaderT Bundle IO Assertion
testActualPrice = do
  testActualPrice' Buy bidExpectations testBookBids
  testActualPrice' Sell askExpectations testBookAsks
  where
    testActualPrice' side expectations bookItems =
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
