{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gdax.Util.Config where

import           Gdax.Util.Config.Internal as C

import           Coinbase.Exchange.Types

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Async  (mapConcurrently)
import           Control.Exception
import           Control.Monad             (replicateM)
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Time.Clock           (NominalDiffTime)
import           GHC.Generics              (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

location :: FilePath
location = "config.yaml"

data Config = Config
    { exchangeConf :: ExchangeConf
    , concurrency  :: Int
    , dataLimit    :: Int
    , pauseGap     :: NominalDiffTime
    , retryGap     :: NominalDiffTime
    }

getGlobalConfig :: ApiType -> IO Config
getGlobalConfig runMode = do
    rawConfig <- getRawConfig location
    let key = (encodeUtf8 . coinbaseKey . credentials) rawConfig
        secret = (encodeUtf8 . coinbaseSecret . credentials) rawConfig
        passphrase = (encodeUtf8 . coinbasePassphrase . credentials) rawConfig
    mgr <- newManager tlsManagerSettings
    exchangeConf <-
        case mkToken key secret passphrase of
            Left err    -> error err
            Right token -> return $ ExchangeConf mgr (Just token) runMode
    return $
        Config
        { exchangeConf = exchangeConf
        , concurrency = (C.concurrency . api) rawConfig
        , dataLimit = (C.dataLimit . api) rawConfig
        , pauseGap = (fromRational . realToFrac . C.pauseGap . api) rawConfig
        , retryGap = (fromRational . realToFrac . C.retryGap . api) rawConfig
        }
