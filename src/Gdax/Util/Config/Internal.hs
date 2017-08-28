{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gdax.Util.Config.Internal where

import           Paths_gdax_trade           (getDataFileName)

import           Gdax.Data.TimeSeries.Types

import           Data.Aeson                 (genericParseJSON)
import           Data.Aeson.Types           (camelTo2, defaultOptions,
                                             fieldLabelModifier)
import           Data.ByteString            (ByteString)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe                 (fromJust)
import           Data.Scientific
import           Data.Text                  (Text)
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Yaml                  as Y
import           GHC.Generics               (Generic)

data RawConfig = RawConfig
    { credentials :: RawCredentialsConfig
    , api         :: RawApiConfig
    , server      :: RawServerConfig
    , log         :: RawLogConfig
    , products    :: HashMap String RawProductConfig
    } deriving (FromJSON, Generic)

data RawCredentialsConfig = RawCredentialsConfig
    { coinbaseKey        :: Text
    , coinbaseSecret     :: Text
    , coinbasePassphrase :: Text
    } deriving (Generic)

instance FromJSON RawCredentialsConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data RawApiConfig = RawApiConfig
    { granularity :: Int
    , mode        :: String
    , throttle    :: RawThrottleConfig
    } deriving (FromJSON, Generic)

data RawServerConfig = RawServerConfig
    { port     :: Int
    , user     :: String
    , password :: String
    } deriving (FromJSON, Generic)

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

data RawProductConfig = RawProductConfig
    { takerFee :: Scientific
    , makerFee :: Scientific
    } deriving (Generic)

instance FromJSON RawProductConfig where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

getRawConfig :: FilePath -> IO RawConfig
getRawConfig path = fromJust <$> decodeFile path
