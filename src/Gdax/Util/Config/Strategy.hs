module Gdax.Util.Config.Strategy where

data StrategyConf = StrategyConf
  { tolerance          :: Double
  , scalpingPercentile :: Double
  } deriving (Eq, Show)
