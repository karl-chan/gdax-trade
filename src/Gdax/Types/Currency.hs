{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gdax.Types.Currency where

import           Data.Hashable
import           GHC.Generics

data Currency
    = USD
    | GBP
    | EUR
    | BTC
    | ETH
    | LTC
    deriving (Eq, Show, Read, Ord, Generic, Hashable)

isFiat :: Currency -> Bool
isFiat c = c `elem` [USD, GBP, EUR]
