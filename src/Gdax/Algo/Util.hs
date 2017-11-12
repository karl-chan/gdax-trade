{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Util where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries

import           Control.Monad.Reader
import           Data.HashMap.Strict    ((!))
import           Prelude                hiding (product)

data ProductBundle = ProductBundle
  { book     :: OrderBook
  , series   :: TimeSeries
  , balance1 :: Balance
  , balance2 :: Balance
  }

extractProductBundle :: Product -> ReaderT Bundle IO ProductBundle
extractProductBundle product@(Pair c1 c2) = do
  Bundle {..} <- ask
  return
    ProductBundle
    { book = books ! product
    , series = multiSeries ! product
    , balance1 = getBalance c1 account
    , balance2 = getBalance c2 account
    }
