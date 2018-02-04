module Gdax.Test.Data.Account where

import           Gdax.Test.Data.Product

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Types.Product

import qualified Data.HashMap.Strict    as HM

testAccount :: MyAccount
testAccount =
  let Pair c1 c2 = testProduct
      balance1 = Balance {currency = c1, total = 2, available = 1, held = 1}
      balance2 =
        Balance {currency = c2, total = 200, available = 100, held = 100}
  in MyAccount
     { balances = HM.fromList [(c1, balance1), (c2, balance2)]
     , orders = HM.empty
     }
