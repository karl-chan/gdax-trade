module Gdax.Types.Currency where

data Currency
    = USD
    | GBP
    | EUR
    | BTC
    | ETH
    | LTC
    deriving (Eq, Show, Read)

isFiat :: Currency -> Bool
isFiat c = c `elem` [USD, GBP, EUR]