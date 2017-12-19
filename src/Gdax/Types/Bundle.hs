{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Types.Bundle where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Types.OrderBook
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries
import           Gdax.Types.Trades
import           Gdax.Util.Config       as Config

import           Data.HashMap.Strict    (HashMap, (!))
import           Data.List              (intercalate)
import           Prelude                hiding (product)

data Bundle = Bundle
  { account     :: MyAccount
  , books       :: HashMap Product OrderBook
  , multiSeries :: HashMap Product TimeSeries
  , multiTrades :: HashMap Product Trades
  , config      :: Config
  }

data ProductBundle = ProductBundle
  { product        :: Product
  , book           :: OrderBook
  , series         :: TimeSeries
  , trades         :: Trades
  , balance1       :: Balance
  , balance2       :: Balance
  , strategyConfig :: StrategyConf
  }

instance Show Bundle where
  show Bundle {..} =
    "Bundle {" ++
    (intercalate
       ", "
       [ "account = " ++ show account
       , "books = " ++ show books
       , "multiSeries = " ++ show multiSeries
       , "multiTrades = " ++ show multiTrades
       ]) ++
    "}"

toProductBundle :: Product -> Bundle -> ProductBundle
toProductBundle product@(Pair c1 c2) Bundle {..} =
  ProductBundle
  { product = product
  , book = books ! product
  , series = multiSeries ! product
  , trades = multiTrades ! product
  , balance1 = getBalance c1 account
  , balance2 = getBalance c2 account
  , strategyConfig = strategyConf config
  }
