module Gdax.Types.Amount where

import           Coinbase.Exchange.Types.Core

data Amount
    = Size Size
    | Price Price
    deriving (Show)
