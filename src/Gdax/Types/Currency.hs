{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Types.Currency where

import           Coinbase.Exchange.Types.Core

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Hashable
import           Data.String.Conversions
import           GHC.Generics

data Currency
    = USD
    | EUR
    | BTC
    | ETH
    | LTC
    deriving (Eq, Show, Read, Ord, Generic, Hashable, Data, NFData)

isFiat :: Currency -> Bool
isFiat c = c `elem` [USD, EUR]

fromId :: CurrencyId -> Currency
fromId = read . show

toId :: Currency -> CurrencyId
toId currency = CurrencyId $ cs . show $ currency
