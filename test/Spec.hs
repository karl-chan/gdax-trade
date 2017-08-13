{-# LANGUAGE OverloadedStrings #-}

import qualified Gdax.Data.OrderBook.Test       as OrderBook
import           Gdax.Data.Product
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
    testFeed <- newProductFeed testCurrencyPair
    defaultMain $ tests testCurrencyPair testConf testFeed

tests :: ProductId -> ExchangeConf -> ProductFeed -> TestTree
tests productId conf feed = testGroup "All tests" [OrderBook.test productId conf feed]
