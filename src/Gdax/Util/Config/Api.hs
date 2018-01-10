module Gdax.Util.Config.Api where

import           Gdax.Types.TimeSeries

import           Gdax.Util.Config.Api.Throttle

data ApiConf = ApiConf
  { decimalPlaces :: Int
  , granularity   :: Granularity
  , mode          :: String
  , throttleConf  :: ThrottleConf
  }
