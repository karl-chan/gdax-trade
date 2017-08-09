{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gdax.Util.Auth
    ( getConf
    ) where

import           Coinbase.Exchange.Types
import           Data.Aeson              (FromJSON, eitherDecode,
                                          genericParseJSON, parseJSON)
import           Data.Aeson.Types        (camelTo2, defaultOptions,
                                          fieldLabelModifier)
import qualified Data.ByteString.Char8   as C (pack)
import qualified Data.ByteString.Lazy    as BL (readFile)
import           Data.Either             (Either (..))
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Paths_gdax_trade        (getDataFileName)

data GdaxCredentials = GdaxCredentials
    { coinbaseKey        :: String
    , coinbaseSecret     :: String
    , coinbasePassphrase :: String
    } deriving (Show, Generic)

instance FromJSON GdaxCredentials where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

credentialsFile :: IO FilePath
credentialsFile = getDataFileName "gdaxCredentials.json"

readCredentialsFile :: FilePath -> IO GdaxCredentials
readCredentialsFile filepath = do
    contents <- BL.readFile filepath
    case eitherDecode contents of
        Left err              -> error err
        Right gdaxCredentials -> return gdaxCredentials

getConf :: ApiType -> IO ExchangeConf
getConf apiType = do
    mgr <- newManager tlsManagerSettings
    gdaxCredentials <- credentialsFile >>= readCredentialsFile
    let key = C.pack $ coinbaseKey gdaxCredentials
        secret = C.pack $ coinbaseSecret gdaxCredentials
        passphrase = C.pack $ coinbasePassphrase gdaxCredentials
        eitherToken = mkToken key secret passphrase
    case eitherToken of
        Left err    -> error err
        Right token -> return $ ExchangeConf mgr (Just token) apiType
