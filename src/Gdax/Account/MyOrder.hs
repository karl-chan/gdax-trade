module Gdax.Account.MyOrder where

import           Gdax.Algo.Action

import           Coinbase.Exchange.Types.Core hiding (Limit, Market, Open)

import           Prelude                      hiding (product)

data MyOrder = MyOrder
  { orderId :: OrderId
  , action  :: Action
  } deriving (Show)
