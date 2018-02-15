module Gdax.Util.Config.Api where

import           Gdax.Util.Config.Api.Throttle
import           Gdax.Util.Time

data ApiConf = ApiConf
  { decimalPlaces :: Int
  , granularity   :: Granularity
  , mode          :: String
  , throttleConf  :: ThrottleConf
  } deriving (Eq, Show)
