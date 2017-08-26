module Gdax.Util.Network where

import           Gdax.Util.Config

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

import Control.Exception
import           Control.Monad.Reader
import qualified Data.ByteString.Char8   as C
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Text               (Text)
import           Network.HTTP.Conduit

gdaxRequest :: String -> String -> ReaderT Config IO (Either SomeException ByteString)
gdaxRequest method url = do
    conf <- reader exchangeConf
    liftIO . try $ execExchange conf $ coinbaseRequest (C.pack method) True url voidBody >>= handleResponse
  where
    handleResponse res = responseBody res $$+- sinkLbs
