{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Config
  ( module Gdax.Util.Config
  , module Gdax.Util.Config.Api
  , module Gdax.Util.Config.Api.Throttle
  , module Gdax.Util.Config.Fees
  , module Gdax.Util.Config.Log
  , module Gdax.Util.Config.Server
  , module Gdax.Util.Config.Strategy
  , module Gdax.Util.Config.Trades
  ) where

import           Gdax.Util.Config.Api
import           Gdax.Util.Config.Api.Throttle
import           Gdax.Util.Config.Fees
import           Gdax.Util.Config.Internal.Env
import           Gdax.Util.Config.Internal.Yaml as Y
import           Gdax.Util.Config.Log
import           Gdax.Util.Config.Server
import           Gdax.Util.Config.Strategy
import           Gdax.Util.Config.Trades

import           Coinbase.Exchange.Types

import           Data.Char
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe
import           Data.Time.Clock
import           Network.HTTP.Client            hiding (port)
import           Network.HTTP.Client.TLS
import           Prelude                        hiding (log, product)

data Config = Config
  { exchangeConf        :: ExchangeConf
  , liveExchangeConf    :: ExchangeConf
  , sandboxExchangeConf :: ExchangeConf
  , apiConf             :: ApiConf
  , bundleRefreshRate   :: NominalDiffTime
  , accountRefreshRate  :: NominalDiffTime
  , serverConf          :: ServerConf
  , tradesConf          :: TradesConf
  , strategyConf        :: StrategyConf
  , feesConf            :: FeesConf
  , logConf             :: LogConf
  }

getGlobalConfig :: IO Config
getGlobalConfig = do
  envConfig <- getEnvConfig
  yamlConfig <- getYamlConfig
  liveExchangeConf <- toExchangeConf Live envConfig
  sandboxExchangeConf <- toExchangeConf Sandbox envConfig
  let exchangeConf =
        case map toLower (Y.mode . api $ yamlConfig) of
          "live" -> liveExchangeConf
          _      -> sandboxExchangeConf
  return
    Config
    { exchangeConf = exchangeConf
    , liveExchangeConf = liveExchangeConf
    , sandboxExchangeConf = sandboxExchangeConf
    , apiConf = toApiConf . api $ yamlConfig
    , bundleRefreshRate =
        let YamlBundleConfig {..} = bundle yamlConfig
        in realToFrac refreshRate
    , accountRefreshRate =
        let YamlAccountConfig {..} = account yamlConfig
        in realToFrac refreshRate
    , serverConf = toServerConf $ server envConfig
    , tradesConf = toTradesConf $ trades yamlConfig
    , strategyConf = toStrategyConf $ strategy yamlConfig
    , feesConf = toFeesConf $ fees yamlConfig
    , logConf = toLogConf $ log yamlConfig
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

toApiConf :: YamlApiConfig -> ApiConf
toApiConf YamlApiConfig {..} =
  ApiConf
  { decimalPlaces = decimalPlaces
  , granularity = realToFrac granularity
  , mode = mode
  , throttleConf =
      let YamlThrottleConfig {..} = throttle
      in ThrottleConf
         { parallelism = parallelism
         , dataLimit = dataLimit
         , interval = realToFrac interval
         , retryGap = realToFrac retryGap
         }
  }

toServerConf :: EnvServerConfig -> ServerConf
toServerConf EnvServerConfig {..} =
  ServerConf
  { maybeUsername = maybeUsername
  , maybePassword = maybePassword
  , herokuKey = herokuKey
  , port = port
  }

toTradesConf :: YamlTradesConfig -> TradesConf
toTradesConf YamlTradesConfig {..} =
  TradesConf {rollingWindow = realToFrac rollingWindow}

toStrategyConf :: YamlStrategyConfig -> StrategyConf
toStrategyConf YamlStrategyConfig {..} =
  StrategyConf {tolerance = tolerance, scalpingPercentile = percentile scalping}

toFeesConf :: HashMap String YamlFeeConfig -> FeesConf
toFeesConf rawFeesConf =
  let transform (k, YamlFeeConfig {..}) =
        let product = read k
        in (product, (maker, taker))
  in HM.fromList . map transform . HM.toList $ rawFeesConf

toLogConf :: YamlLogConfig -> LogConf
toLogConf YamlLogConfig {..} =
  LogConf
  {logFile = Nothing, enableStderr = True, logLevel = parseLogLevel level}
