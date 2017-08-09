{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

import Gdax.Trade.Auth (getConf)
import Gdax.Trade.Feed (Feed, newFeed)
import qualified Gdax.Trade.OrderBook.Test      as OrderBook

testRunMode :: ApiType
testRunMode = Live

testCurrencyPair :: ProductId
testCurrencyPair = "BTC-USD"

main :: IO ()
main = do
    testConf <- getConf testRunMode
    testFeed <- newFeed testCurrencyPair
    defaultMain $ tests testCurrencyPair testConf testFeed

tests :: ProductId -> ExchangeConf -> Feed -> TestTree
tests productId conf feed = testGroup "All tests" [OrderBook.test productId conf feed]
