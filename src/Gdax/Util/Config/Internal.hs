{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gdax.Util.Config.Internal where

import           Data.Aeson          (genericParseJSON)
import           Data.Aeson.Types    (camelTo2, defaultOptions,
                                      fieldLabelModifier)
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import           Data.Yaml           as Y
import           GHC.Generics        (Generic)

data RawConfig = RawConfig
    { credentials :: RawCredentialsConfig
    , api         :: RawApiConfig
    , bundle      :: RawBundleConfig
    , server      :: RawServerConfig
    , log         :: RawLogConfig
    , fees        :: HashMap String RawFeeConfig
    } deriving (FromJSON, Generic)

data RawCredentialsConfig = RawCredentialsConfig
    { coinbaseKey        :: Text
    , coinbaseSecret     :: Text
    , coinbasePassphrase :: Text
    } deriving (Generic)

instance FromJSON RawCredentialsConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data RawApiConfig = RawApiConfig
    { granularity :: Double
    , mode        :: String
    , throttle    :: RawThrottleConfig
    } deriving (FromJSON, Generic)

newtype RawBundleConfig = RawBundleConfig
    { refreshRate :: Double
    } deriving (Generic)

instance FromJSON RawBundleConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data RawServerConfig = RawServerConfig
    { defaultPort :: Int
    , user        :: String
    , password    :: String
    } deriving (Generic)

instance FromJSON RawServerConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data RawThrottleConfig = RawThrottleConfig
    { concurrency :: Int
    , dataLimit   :: Int
    , pauseGap    :: Double
    , retryGap    :: Double
    } deriving (Generic)

instance FromJSON RawThrottleConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype RawLogConfig = RawLogConfig
    { level :: String
    } deriving (Generic, FromJSON)

data RawFeeConfig = RawFeeConfig
    { taker :: Double
    , maker :: Double
    } deriving (Generic, FromJSON)

getRawConfig :: FilePath -> IO RawConfig
getRawConfig path = fromJust <$> decodeFile path
