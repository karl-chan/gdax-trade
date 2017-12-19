module Gdax.Util.Config.Strategy where

data StrategyConf = StrategyConf
  { tolerance             :: Double
  , scalpMarginPercentile :: Double
  }
