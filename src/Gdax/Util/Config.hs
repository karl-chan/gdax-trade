{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Util.Config where

import           Gdax.Data.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Util.Config.Internal

import           Coinbase.Exchange.Types

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception
import           Control.Monad              (replicateM)
import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Scientific
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
    , productsConf           :: HashMap Product ProductConf
    }

data ProductConf = ProductConf
    { product  :: Product
    , makerFee :: Scientific
    , takerFee :: Scientific
    }

getGlobalConfig :: IO Config
getGlobalConfig = do
    rawConfig <- getRawConfig location
    exchangeConf <- toExchangeConf (toRunMode $ api rawConfig) (credentials rawConfig)
    initLogging $ (level . log) rawConfig
    return
        Config
        { exchangeConf = exchangeConf
        , apiGranularity = (fromIntegral . granularity . api) rawConfig
        , apiThrottleConcurrency = (concurrency . throttle . api) rawConfig
        , apiThrottleDataLimit = (dataLimit . throttle . api) rawConfig
        , apiThrottlePauseGap = (fromRational . realToFrac . pauseGap . throttle . api) rawConfig
        , apiThrottleRetryGap = (fromRational . realToFrac . retryGap . throttle . api) rawConfig
        , serverConf = (toServerConf . server) rawConfig
        , serverUser = (user . server) rawConfig
        , serverPassword = (password . server) rawConfig
        , productsConf = (toProductsConf . products) rawConfig
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

toProductsConf :: HashMap String RawProductConfig -> HashMap Product ProductConf
toProductsConf productDetails =
    let transform (k, RawProductConfig {..}) =
            let product = read k
            in (product, ProductConf product makerFee takerFee)
    in Map.fromList . map transform . Map.toList $ productDetails

initLogging :: String -> IO ()
initLogging s = do
    let logLevel = read $ map toUpper s
    stdOutHandler <-
        streamHandler stdout logLevel >>= \lh ->
            return $ setFormatter lh (simpleLogFormatter "[$loggername:$time] $msg")
    updateGlobalLogger rootLoggerName (setLevel logLevel . setHandlers [stdOutHandler])
