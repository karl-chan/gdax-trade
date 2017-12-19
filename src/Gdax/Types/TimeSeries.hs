{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Types.TimeSeries where

import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Core (Price (..), Size (..))

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Map                     (Map)
import           Data.Time.Clock
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (max, min, product)

data Stat = Stat
  { start   :: UTCTime
  , end     :: UTCTime
  , low     :: Price
  , high    :: Price
  , open    :: Price
  , close   :: Price
  , volume  :: Size
  , product :: Product
  } deriving (Eq, Show, Data, Typeable, Generic, NFData)

type StartTime = UTCTime

type EndTime = UTCTime

type TimeSeries = Map StartTime Stat

type Granularity = NominalDiffTime

data Direction
  = Up
  | Down
  | None
  deriving (Eq, Show, Read, Data, Typeable, Generic, NFData)
