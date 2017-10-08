{-# LANGUAGE RecordWildCards #-}

module Gdax.Account.MyOrder where

import           Gdax.Algo.Action

import           Coinbase.Exchange.Types.Core   hiding (Limit, Market, Open)

data MyOrder = MyOrder
    { orderId :: OrderId
    , action  :: Action
    }