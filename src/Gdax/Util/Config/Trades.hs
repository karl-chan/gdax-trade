module Gdax.Util.Config.Trades where

import           Data.Time.Clock

data TradesConf = TradesConf
  { rollingWindow :: NominalDiffTime
  }
