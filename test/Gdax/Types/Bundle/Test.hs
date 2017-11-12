module Gdax.Types.Bundle.Test where

import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook.Test
import           Gdax.Types.Product.Test
import           Gdax.Util.Config.Test

import qualified Data.HashMap.Strict       as HM

testBundle :: Bundle
testBundle =
  Bundle
    -- account =
  { books = HM.fromList [(testProduct, testOrderBook)]
    -- multiSeries =
  , config = testConfig
  }
