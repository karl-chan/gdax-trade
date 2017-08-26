{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Data.TimeSeries.Types where

import           Coinbase.Exchange.Types.Core (Price (..), Size (..))

import           BroadcastChan.Throw          (BroadcastChan, In)
import           Control.Concurrent           (threadDelay)
import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Time.Clock              (NominalDiffTime, UTCTime)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (max, min)

data Stat = Stat
    { start  :: UTCTime
    , end    :: UTCTime
    , low    :: Price
    , high   :: Price
    , open   :: Price
    , close  :: Price
    , volume :: Size
    } deriving (Eq, Show, Data, Typeable, Generic, NFData)

type TimeSeries = Map StartTime Stat

type TimeSeriesFeed = BroadcastChan In TimeSeries

type StartTime = UTCTime

type EndTime = UTCTime

type Granularity = NominalDiffTime

toSeconds :: Granularity -> Double
toSeconds = realToFrac
