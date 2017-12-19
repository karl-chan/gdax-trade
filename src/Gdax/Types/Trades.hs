module Gdax.Types.Trades where

import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Core

import           Data.Time.Clock

type Trades = [Trade]

data Trade = Trade
  { time    :: UTCTime
  , product :: Product
  , side    :: Side
  , size    :: Size
  , price   :: Price
  } deriving (Eq, Show)
