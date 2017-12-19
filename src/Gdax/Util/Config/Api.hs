module Gdax.Util.Config.Api where

import Gdax.Util.Config.Api.Throttle

data ApiConf = ApiConf
  { decimalPlaces :: Int
  , granularity :: Double
  , mode :: String
  , throttleConf :: ThrottleConf
  }
