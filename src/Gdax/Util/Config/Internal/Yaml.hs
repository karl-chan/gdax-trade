{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gdax.Util.Config.Internal.Yaml where

import           Control.Exception
import           Data.Aeson          (genericParseJSON)
import           Data.Aeson.Types    (camelTo2, defaultOptions,
                                      fieldLabelModifier)
import           Data.HashMap.Strict (HashMap)
import           Data.Yaml           as Y
import           GHC.Generics        (Generic)
import           Paths_gdax_trade

configLocation :: FilePath
configLocation = "data/config.yaml"

data YamlConfig = YamlConfig
  { api        :: YamlApiConfig
  , bundle     :: YamlBundleConfig
  , account    :: YamlAccountConfig
  , timeSeries :: YamlTimeSeriesConfig
  , trades     :: YamlTradesConfig
  , strategy   :: YamlStrategyConfig
  , log        :: YamlLogConfig
  , fees       :: HashMap String YamlFeeConfig
  } deriving (Generic)

instance FromJSON YamlConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data YamlApiConfig = YamlApiConfig
  { decimalPlaces :: Int
  , granularity   :: Double
  , mode          :: String
  , throttle      :: YamlThrottleConfig
  } deriving (Generic)

instance FromJSON YamlApiConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype YamlBundleConfig = YamlBundleConfig
  { refreshRate :: Double
  } deriving (Generic)

instance FromJSON YamlBundleConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype YamlAccountConfig = YamlAccountConfig
  { refreshRate :: Double
  } deriving (Generic)

instance FromJSON YamlAccountConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype YamlTimeSeriesConfig = YamlTimeSeriesConfig
  { initialPeriod :: Double
  } deriving (Generic)

instance FromJSON YamlTimeSeriesConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype YamlTradesConfig = YamlTradesConfig
  { rollingWindow :: Double
  } deriving (Generic)

instance FromJSON YamlTradesConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data YamlStrategyConfig = YamlStrategyConfig
  { tolerance :: Double
  , scalping  :: YamlScalpingConfig
  } deriving (Generic, FromJSON)

newtype YamlScalpingConfig = YamlScalpingConfig
  { percentile :: Double
  } deriving (Generic, FromJSON)

data YamlThrottleConfig = YamlThrottleConfig
  { parallelism :: Int
  , dataLimit   :: Int
  , interval    :: Double
  , retryGap    :: Double
  } deriving (Generic)

instance FromJSON YamlThrottleConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype YamlLogConfig = YamlLogConfig
  { level :: String
  } deriving (Generic, FromJSON)

data YamlFeeConfig = YamlFeeConfig
  { taker :: Double
  , maker :: Double
  } deriving (Generic, FromJSON)

getYamlConfig :: IO YamlConfig
getYamlConfig = do
  eitherConfig <- decodeFileEither =<< getDataFileName configLocation
  case eitherConfig of
    Left err         -> throw err
    Right yamlConfig -> return yamlConfig
