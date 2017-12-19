{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
configLocation = "config.yaml"

data YamlConfig = YamlConfig
  { api      :: YamlApiConfig
  , bundle   :: YamlBundleConfig
  , strategy :: YamlStrategyConfig
  , log      :: YamlLogConfig
  , fees     :: HashMap String YamlFeeConfig
  } deriving (FromJSON, Generic)

data YamlApiConfig = YamlApiConfig
  { decimalPlaces :: Int
  , granularity   :: Double
  , mode          :: String
  , throttle      :: YamlThrottleConfig
  } deriving (Generic)

instance FromJSON YamlApiConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data YamlBundleConfig = YamlBundleConfig
  { refreshRate :: Double
  } deriving (Generic)

instance FromJSON YamlBundleConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data YamlStrategyConfig = YamlStrategyConfig
  { tolerance             :: Double
  , scalpMarginPercentile :: Double
  } deriving (Generic)

instance FromJSON YamlStrategyConfig where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

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
