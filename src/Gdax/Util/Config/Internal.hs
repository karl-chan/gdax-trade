{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Gdax.Util.Config.Internal where

import           Paths_gdax_trade           (getDataFileName)

import           Gdax.Data.TimeSeries.Types

import           Data.Aeson                 (genericParseJSON)
import           Data.Aeson.Types           (camelTo2, defaultOptions,
                                             fieldLabelModifier)
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Yaml                  (FromJSON (..), decode, decodeFile,
                                             parseJSON, (.:))
import qualified Data.Yaml                  as Y
import           GHC.Generics               (Generic)

data RawConfig = RawConfig
    { credentials :: RawCredentialsConfig
    , api         :: RawApiConfig
    , server      :: RawServerConfig
    , log         :: RawLogConfig
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

data RawLogConfig = RawLogConfig
    { level :: String
    } deriving (Generic, FromJSON)

getRawConfig :: FilePath -> IO RawConfig
getRawConfig path = fmap fromJust $ decodeFile path
