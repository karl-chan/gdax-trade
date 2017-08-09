{-# LANGUAGE OverloadedStrings #-}

import qualified Gdax.Data.OrderBook.Test       as OrderBook
import           Gdax.Util.Auth
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

import           Test.Tasty

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
