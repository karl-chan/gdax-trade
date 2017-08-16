{-# LANGUAGE OverloadedStrings #-}

import qualified Gdax.Data.OrderBook.Test       as OrderBook
import           Gdax.Data.Product
import           Gdax.Util.Config

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Test.Tasty

testRunMode :: ApiType
testRunMode = Live

testProduct :: ProductId
testProduct = "BTC-USD"

main :: IO ()
main = do
    testConfig <- getGlobalConfig testRunMode
    testFeed <- newProductFeed [testProduct]
    defaultMain $ tests testProduct testFeed testConfig

tests :: ProductId -> ProductFeed -> Config -> TestTree
tests productId productFeed config = do
    testGroup "All tests" [OrderBook.test productId productFeed config]
