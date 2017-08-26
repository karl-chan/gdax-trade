module Gdax.Algo.Types where

import           Gdax.Account.MyAccount
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.TimeSeries.Types

import           Coinbase.Exchange.Types.Core (Price, Side)

type Algorithm = OrderBook -> TimeSeries -> MyAccount -> Decision

type LimitPrice = Price

type StopPrice = Price

data Decision
    = Market Side
    | Limit Side
            Price
    | Stop Side
           LimitPrice
           StopPrice
    | NoAction
