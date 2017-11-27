module Gdax.Util.Config.Env where

import           Control.Monad
import           Data.ByteString         (ByteString)
import           Data.Maybe
import           Data.String.Conversions
import           Data.UUID
import           System.Environment

defaultPort :: Int
defaultPort = 8080

data EnvConfig = EnvConfig
  { liveCredentials    :: EnvCredentialsConfig
  , sandboxCredentials :: EnvCredentialsConfig
  , server             :: EnvServerConfig
  } deriving (Show)

data EnvCredentialsConfig = EnvCredentialsConfig
  { key        :: ByteString
  , secret     :: ByteString
  , passphrase :: ByteString
  } deriving (Show)

data EnvServerConfig = EnvServerConfig
  { maybeUsername :: Maybe String
  , maybePassword :: Maybe String
  , herokuKey     :: UUID
  , port          :: Int
  } deriving (Show)

getEnvConfig :: IO EnvConfig
getEnvConfig = do
  maybeKey <- lookupEnv "GDAX_KEY"
  maybeSecret <- lookupEnv "GDAX_SECRET"
  maybePassphrase <- lookupEnv "GDAX_PASSPHRASE"
  when (isNothing maybeKey) $
    error "GDAX_KEY environmental variable is missing. Please see README.id"
  when (isNothing maybeSecret) $
    error "GDAX_SECRET environmental variable is missing. Please see README.md"
  when (isNothing maybePassphrase) $
    error
      "GDAX_PASSPHRASE environmental variable is missing. Please see README.md"
  maybeSandboxKey <- lookupEnv "GDAX_SANDBOX_KEY"
  maybeSandboxSecret <- lookupEnv "GDAX_SANDBOX_SECRET"
  maybeSandboxPassphrase <- lookupEnv "GDAX_SANDBOX_PASSPHRASE"
  when (isNothing maybeSandboxKey) $
    error
      "GDAX_SANDBOX_KEY environmental variable is missing. Please see README.id"
  when (isNothing maybeSandboxSecret) $
    error
      "GDAX_SANDBOX_SECRET environmental variable is missing. Please see README.md"
  when (isNothing maybeSandboxPassphrase) $
    error
      "GDAX_SANDBOX_PASSPHRASE environmental variable is missing. Please see README.md"
  maybeServerUser <- lookupEnv "GDAX_SERVER_USER"
  maybeServerPass <- lookupEnv "GDAX_SERVER_PASS"
  maybeServerPort <- lookupEnv "GDAX_SERVER_PORT"
  maybeServerFallbackPort <- lookupEnv "PORT"
  maybeServerHerokuKey <- lookupEnv "GDAX_SERVER_HEROKU_KEY"
  when (isNothing maybeServerHerokuKey) $
    error
      "GDAX_SERVER_HEROKU_KEY environmental variable is missing. Please see README.md"
  let key = cs . fromJust $ maybeKey
      secret = cs . fromJust $ maybeSecret
      passphrase = cs . fromJust $ maybePassphrase
      sandboxKey = cs . fromJust $ maybeSandboxKey
      sandboxSecret = cs . fromJust $ maybeSandboxSecret
      sandboxPassphrase = cs . fromJust $ maybeSandboxPassphrase
      port =
        case maybeServerPort of
          Nothing ->
            case maybeServerFallbackPort of
              Nothing              -> defaultPort
              Just fallbackPortStr -> read fallbackPortStr
          Just portStr -> read portStr
      herokuKey =
        case fromString . fromJust $ maybeServerHerokuKey of
          Nothing ->
            error $
            "Expected GDAX_SERVER_HEROKU_KEY to be a valid UUID key, but got: " ++
            fromJust maybeServerHerokuKey
          Just uuid -> uuid
  return
    EnvConfig
    { liveCredentials = EnvCredentialsConfig key secret passphrase
    , sandboxCredentials =
        EnvCredentialsConfig sandboxKey sandboxSecret sandboxPassphrase
    , server = EnvServerConfig maybeServerUser maybeServerPass herokuKey port
    }
