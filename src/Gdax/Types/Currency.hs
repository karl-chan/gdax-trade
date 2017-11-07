{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Gdax.Types.Currency where

import           Coinbase.Exchange.Types.Core

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Either.Combinators
import           Data.Hashable
import           Data.String.Conversions
import           GHC.Generics
import           Text.Read

data Currency
    = USD
    | GBP
    | EUR
    | BTC
    | ETH
    | LTC
    deriving (Eq, Show, Read, Ord, Generic, Hashable, Data, NFData)

isFiat :: Currency -> Bool
isFiat c = c `elem` [USD, EUR]

fromId :: CurrencyId -> Currency
fromId CurrencyId {..} = fromRight' $ safeRead . cs $ unCurrencyId

toId :: Currency -> CurrencyId
toId currency = CurrencyId $ cs . show $ currency

safeRead :: String -> Either String Currency
safeRead arg =
    case readMaybe arg of
        Nothing       -> Left $ arg ++ " could not be parsed as a currency."
        Just currency -> Right currency
