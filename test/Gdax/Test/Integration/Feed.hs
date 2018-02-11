module Gdax.Test.Integration.Feed where

import qualified Gdax.Test.Integration.Feed.OrderBook  as OrderBook
import qualified Gdax.Test.Integration.Feed.TimeSeries as TimeSeries
import qualified Gdax.Test.Integration.Feed.Trades     as Trades

import           Test.Tasty

tests :: TestTree
tests = testGroup "Feed" [OrderBook.tests, TimeSeries.tests, Trades.tests]
