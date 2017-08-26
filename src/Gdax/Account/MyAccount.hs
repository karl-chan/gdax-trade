module Gdax.Account.MyAccount where

import           Data.HashMap
import           Gdax.Account.Balance
import           Gdax.Types.Currency
import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Private (Order)

data MyAccount = MyAccount
    { balances :: Map Currency Balance
    , orders   :: [Order]
    }
