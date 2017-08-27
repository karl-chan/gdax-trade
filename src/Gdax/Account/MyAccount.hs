module Gdax.Account.MyAccount where

import           Gdax.Account.Balance
import           Gdax.Types.Currency
import           Gdax.Types.Order
import           Gdax.Types.Product

import           Data.HashMap         (Map, (!))
import qualified Data.HashMap         as Map
import           Data.Maybe

data MyAccount = MyAccount
    { balances :: Map Currency Balance
    , orders   :: [Order]
    }

getBalance :: Currency -> MyAccount -> Balance
getBalance currency account = balances account ! currency
