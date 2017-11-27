{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Config where

import           Gdax.Types.TimeSeries
import           Gdax.Util.Config.Env
import           Gdax.Util.Config.Fees
import           Gdax.Util.Config.Log
import           Gdax.Util.Config.Yaml

import           Coinbase.Exchange.Types

import           Data.Char
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           Data.Time.Clock         (NominalDiffTime)
import           Data.UUID
import           Network.HTTP.Client     hiding (port)
import           Network.HTTP.Client.TLS
import           Prelude                 hiding (log, product)

data Config = Config
  { exchangeConf           :: ExchangeConf
  , liveExchangeConf       :: ExchangeConf
  , sandboxExchangeConf    :: ExchangeConf
  , apiDecimalPlaces       :: Int
  , apiGranularity         :: Granularity
  , apiThrottleParallelism :: Int
  , apiThrottleDataLimit   :: Int
  , apiThrottleInterval    :: NominalDiffTime
  , apiThrottleRetryGap    :: NominalDiffTime
  , bundleRefreshRate      :: NominalDiffTime
  , serverConf             :: ServerConf
  , feesConf               :: FeesConf
  , logConf                :: LogConfig
  }

data ServerConf = ServerConf
  { maybeUsername :: Maybe String
  , maybePassword :: Maybe String
  , herokuKey     :: UUID
  , port          :: Int
  }

getGlobalConfig :: IO Config
getGlobalConfig = do
  envConfig <- getEnvConfig
  yamlConfig <- getYamlConfig
  liveExchangeConf <- toExchangeConf Live envConfig
  sandboxExchangeConf <- toExchangeConf Sandbox envConfig
  let exchangeConf =
        case map toLower (mode . api $ yamlConfig) of
          "live" -> liveExchangeConf
          _      -> sandboxExchangeConf
      serverConf = toServerConf $ server envConfig
  return
    Config
    { exchangeConf = exchangeConf
    , liveExchangeConf = liveExchangeConf
    , sandboxExchangeConf = sandboxExchangeConf
    , apiDecimalPlaces = (decimalPlaces . api) yamlConfig
    , apiGranularity = (realToFrac . granularity . api) yamlConfig
    , apiThrottleParallelism = (parallelism . throttle . api) yamlConfig
    , apiThrottleDataLimit = (dataLimit . throttle . api) yamlConfig
    , apiThrottleInterval = (realToFrac . interval . throttle . api) yamlConfig
    , apiThrottleRetryGap = (realToFrac . retryGap . throttle . api) yamlConfig
    , bundleRefreshRate = (realToFrac . refreshRate . bundle) yamlConfig
    , serverConf = serverConf
    , feesConf = (toFeesConf . fees) yamlConfig
    , logConf = (toLogConf . log) yamlConfig
    }

toRunMode :: YamlApiConfig -> ApiType
toRunMode YamlApiConfig {..} =
  case map toLower mode of
    "live" -> Live
    _      -> Sandbox

toExchangeConf :: ApiType -> EnvConfig -> IO ExchangeConf
toExchangeConf apiType EnvConfig {..} = do
  mgr <- newManager tlsManagerSettings
  let EnvCredentialsConfig {..} =
        case apiType of
          Live    -> liveCredentials
          Sandbox -> sandboxCredentials
  case mkToken key secret passphrase of
    Left err    -> error err
    Right token -> return $ ExchangeConf mgr (Just token) apiType

toServerConf :: EnvServerConfig -> ServerConf
toServerConf EnvServerConfig {..} =
  ServerConf
  { maybeUsername = maybeUsername
  , maybePassword = maybePassword
  , herokuKey = herokuKey
  , port = port
  }

toFeesConf :: HashMap String YamlFeeConfig -> FeesConf
toFeesConf rawFeesConf =
  let transform (k, YamlFeeConfig {..}) =
        let product = read k
        in (product, (maker, taker))
  in HM.fromList . map transform . HM.toList $ rawFeesConf

toLogConf :: YamlLogConfig -> LogConfig
toLogConf YamlLogConfig {..} =
  LogConfig
  {logFile = Nothing, enableStderr = True, logLevel = parseLogLevel level}
