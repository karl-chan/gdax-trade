{-# LANGUAGE RecordWildCards #-}

module Gdax.Types.Bundle where

import           Gdax.Account.MyAccount
import           Gdax.Types.OrderBook
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries
import           Gdax.Util.Config

import           Data.HashMap.Strict    (HashMap, (!))

data Bundle = Bundle
    { account :: MyAccount
    , books   :: HashMap Product OrderBook
    , series  :: HashMap Product TimeSeries
    , config  :: Config
    }

instance Show Bundle where
    show Bundle {..} = "Account: " ++ show account ++ ", Books: " ++ show books ++ ", Series: " ++ show series

timeSeries :: Product -> Bundle -> TimeSeries
timeSeries product Bundle {..} = series ! product

orderBook :: Product -> Bundle -> OrderBook
orderBook product Bundle {..} = books ! product
