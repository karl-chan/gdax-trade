{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Util.Config where

import           Gdax.Data.TimeSeries.Types
import           Gdax.Util.Config.Internal

import           Coinbase.Exchange.Types

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception
import           Control.Monad              (replicateM)
import           Data.Char
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock            (NominalDiffTime)
import           GHC.Generics               (Generic)
import           Happstack.Server           (Conf (..), nullConf)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Prelude                    hiding (log)
import           System.IO                  (stdout)
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (streamHandler)
import           System.Log.Logger          (Priority (..), debugM,
                                             rootLoggerName, setHandlers,
                                             setLevel, updateGlobalLogger)

type ServerConf = Conf

location :: FilePath
location = "config.yaml"

data Config = Config
    { exchangeConf           :: ExchangeConf
    , apiGranularity         :: Granularity
    , apiThrottleConcurrency :: Int
    , apiThrottleDataLimit   :: Int
    , apiThrottlePauseGap    :: NominalDiffTime
    , apiThrottleRetryGap    :: NominalDiffTime
    , serverConf             :: ServerConf
    , serverUser             :: String
    , serverPassword         :: String
    }

getGlobalConfig :: IO Config
getGlobalConfig = do
    rawConfig <- getRawConfig location
    exchangeConf <- toExchangeConf (toRunMode $ api rawConfig) (credentials rawConfig)
    let serverConf = toServerConf $ server rawConfig
    initLogging $ (level . log) rawConfig
    return
        Config
        { exchangeConf = exchangeConf
        , apiGranularity = (fromIntegral . granularity . api) rawConfig
        , apiThrottleConcurrency = (concurrency . throttle . api) rawConfig
        , apiThrottleDataLimit = (dataLimit . throttle . api) rawConfig
        , apiThrottlePauseGap = (fromRational . realToFrac . pauseGap . throttle . api) rawConfig
        , apiThrottleRetryGap = (fromRational . realToFrac . retryGap . throttle . api) rawConfig
        , serverConf = serverConf
        , serverUser = (user . server) rawConfig
        , serverPassword = (password . server) rawConfig
        }

toRunMode :: RawApiConfig -> ApiType
toRunMode RawApiConfig {..} =
    case map toLower mode of
        "live" -> Live
        _      -> Sandbox

toExchangeConf :: ApiType -> RawCredentialsConfig -> IO ExchangeConf
toExchangeConf runMode RawCredentialsConfig {..} = do
    let key = encodeUtf8 coinbaseKey
        secret = encodeUtf8 coinbaseSecret
        passphrase = encodeUtf8 coinbasePassphrase
    mgr <- newManager tlsManagerSettings
    case mkToken key secret passphrase of
        Left err    -> error err
        Right token -> return $ ExchangeConf mgr (Just token) runMode

toServerConf :: RawServerConfig -> ServerConf
toServerConf RawServerConfig {..} = nullConf {port = port}

initLogging :: String -> IO ()
initLogging s = do
    let logLevel = read $ map toUpper s
    stdOutHandler <-
        streamHandler stdout logLevel >>= \lh ->
            return $ setFormatter lh (simpleLogFormatter "[$loggername:$time] $msg")
    updateGlobalLogger rootLoggerName (setLevel logLevel . setHandlers [stdOutHandler])
