module Gdax.Util.Config.Api.Throttle where

import           Data.Time.Clock

data ThrottleConf = ThrottleConf
  { parallelism :: Int
  , dataLimit   :: Int
  , interval    :: NominalDiffTime
  , retryGap    :: NominalDiffTime
  } deriving (Eq, Show)
