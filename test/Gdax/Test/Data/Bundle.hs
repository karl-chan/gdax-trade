module Gdax.Test.Data.Bundle where

import           Gdax.Test.Data.Account
import           Gdax.Test.Data.Config
import           Gdax.Test.Data.OrderBook
import           Gdax.Test.Data.Product
import           Gdax.Test.Data.TimeSeries
import           Gdax.Test.Data.Trades

import           Gdax.Types.Bundle

import qualified Data.HashMap.Strict       as HM

testBundle :: Bundle
testBundle =
  Bundle
  { account = testAccount
  , books = HM.singleton testProduct testOrderBook
  , multiSeries = HM.singleton testProduct testSeries
  , multiTrades = HM.singleton testProduct testTrades
  , config = testConfig
  }
