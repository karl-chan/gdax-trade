{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gdax.Data.TimeSeries.Types where

import           Coinbase.Exchange.Types.Core (Price (..), Size (..))
import           Gdax.Types.Product
import           Gdax.Util.Feed

import           Control.DeepSeq              (NFData)
import           Data.Data                    (Data)
import           Data.Map                     (Map)
import           Data.Time.Clock              (NominalDiffTime, UTCTime)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (max, min)

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

type TimeSeriesFeed = Feed TimeSeries

type TimeSeriesFeedListener = FeedListener TimeSeries

type Granularity = NominalDiffTime
