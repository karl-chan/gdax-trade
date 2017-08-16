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
    { credentials :: RawCredentials
    , api         :: RawApi
    } deriving (FromJSON, Generic)

data RawCredentials = RawCredentials
    { coinbaseKey        :: Text
    , coinbaseSecret     :: Text
    , coinbasePassphrase :: Text
    } deriving (Generic)

instance FromJSON RawCredentials where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data RawApi = RawApi
    { concurrency :: Int
    , dataLimit   :: Int
    , pauseGap    :: Double
    , retryGap    :: Double
    } deriving (Generic)

instance FromJSON RawApi where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

getRawConfig :: FilePath -> IO RawConfig
getRawConfig path = fmap fromJust $ decodeFile path
