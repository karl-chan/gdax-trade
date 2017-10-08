{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Config where

import           Gdax.Data.TimeSeries.Types
import           Gdax.Util.Config.Fees
import           Gdax.Util.Config.Internal

import           Coinbase.Exchange.Types

import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock            (NominalDiffTime)
import           Happstack.Server           (Conf (..), nullConf)
import           Network.HTTP.Client        hiding (port)
import           Network.HTTP.Client.TLS
import           Prelude                    hiding (log, product)
import           System.Environment
import           System.IO                  (stdout)
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (streamHandler)
import           System.Log.Logger          (rootLoggerName, setHandlers,
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
    , bundleRefreshRate      :: NominalDiffTime
    , serverConf             :: ServerConf
    , serverUser             :: String
    , serverPassword         :: String
    , feesConf               :: FeesConf
    }

getGlobalConfig :: IO Config
getGlobalConfig = do
    rawConfig <- getRawConfig location
    exchangeConf <- toExchangeConf (toRunMode $ api rawConfig) (credentials rawConfig)
    serverConf <- toServerConf $ server rawConfig
    initLogging $ (level . log) rawConfig
    return
        Config
        { exchangeConf = exchangeConf
        , apiGranularity = (realToFrac . granularity . api) rawConfig
        , apiThrottleConcurrency = (concurrency . throttle . api) rawConfig
        , apiThrottleDataLimit = (dataLimit . throttle . api) rawConfig
        , apiThrottlePauseGap = (realToFrac . pauseGap . throttle . api) rawConfig
        , apiThrottleRetryGap = (realToFrac . retryGap . throttle . api) rawConfig
        , bundleRefreshRate = (realToFrac . refreshRate . bundle) rawConfig
        , serverConf = serverConf
        , serverUser = (user . server) rawConfig
        , serverPassword = (password . server) rawConfig
        , feesConf = (toFeesConf . fees) rawConfig
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

toServerConf :: RawServerConfig -> IO ServerConf
toServerConf RawServerConfig {..} = do
    envPort <- lookupEnv "PORT"
    return $ nullConf {port = maybe defaultPort read envPort}

toFeesConf :: HashMap String RawFeeConfig -> FeesConf
toFeesConf rawFeesConf =
    let transform (k, RawFeeConfig {..}) =
            let product = read k
            in (product, (maker, taker))
    in HM.fromList . map transform . HM.toList $ rawFeesConf

initLogging :: String -> IO ()
initLogging s = do
    let logLevel = read $ map toUpper s
    stdOutHandler <-
        streamHandler stdout logLevel >>= \lh ->
            return $ setFormatter lh (simpleLogFormatter "[$loggername:$time] $msg")
    updateGlobalLogger rootLoggerName (setLevel logLevel . setHandlers [stdOutHandler])
