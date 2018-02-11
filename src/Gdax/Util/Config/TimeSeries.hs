module Gdax.Util.Config.TimeSeries where

import           Data.Time.Clock

data TimeSeriesConf = TimeSeriesConf
  { initialPeriod :: NominalDiffTime
  }
