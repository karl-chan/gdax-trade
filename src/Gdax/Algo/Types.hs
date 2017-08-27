module Gdax.Algo.Types where

import           Gdax.Account.MyAccount
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Core (Price, Side)

type Algorithm = TimeSeries -> OrderBook -> Prediction

type Prediction = [(Action, ExpectedReturn)]

data Action
    = Market Side
    | Limit Side
            LimitPrice
            (Maybe StopPrice)
    | NoAction
    deriving (Show)

type LimitPrice = Price

type StopPrice = Price

type ExpectedReturn = Double
