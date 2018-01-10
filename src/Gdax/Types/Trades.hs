{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Types.Trades where

import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Core

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Map                     (Map)
import           Data.Time.Clock
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)

type Trades = Map UTCTime Trade

data Trade = Trade
  { time    :: UTCTime
  , product :: Product
  , side    :: Side
  , size    :: Size
  , price   :: Price
  } deriving (Eq, Show, Data, Typeable, Generic, NFData)
