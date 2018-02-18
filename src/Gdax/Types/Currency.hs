{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Gdax.Types.Currency where

import           Coinbase.Exchange.Types.Core

import           Control.DeepSeq              (NFData)
import           Data.Aeson                   (ToJSON)
import           Data.Data                    (Data)
import           Data.Hashable
import           Data.String.Conversions
import           GHC.Generics
import           Text.Read

data Currency
  = USD
  | GBP
  | EUR
  | BTC
  | BCH
  | ETH
  | LTC
  deriving ( Eq
           , Show
           , Enum
           , Bounded
           , Read
           , Ord
           , Generic
           , Hashable
           , Data
           , NFData
           , ToJSON
           )

isFiat :: Currency -> Bool
isFiat c = c `elem` [USD, EUR]

fromId :: CurrencyId -> Currency
fromId CurrencyId {..} =
  case safeRead . cs $ unCurrencyId of
    Left err       -> error err
    Right currency -> currency

toId :: Currency -> CurrencyId
toId currency = CurrencyId $ cs . show $ currency

safeRead :: String -> Either String Currency
safeRead arg =
  case readMaybe arg of
    Nothing       -> Left $ arg ++ " could not be parsed as a currency."
    Just currency -> Right currency
