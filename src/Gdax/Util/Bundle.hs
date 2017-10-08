{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Bundle where

import           Gdax.Account.MyAccount
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Config

import           Data.HashMap.Strict        (HashMap, (!))
import           Prelude                    hiding (product)

data Bundle = Bundle
    { account :: MyAccount
    , books   :: HashMap Product OrderBook
    , series  :: HashMap Product TimeSeries
    , config  :: Config
    }

getTimeSeries :: Bundle -> Product -> TimeSeries
getTimeSeries Bundle {..} product = series ! product

getOrderBook :: Bundle -> Product -> OrderBook
getOrderBook Bundle {..} product = books ! product
