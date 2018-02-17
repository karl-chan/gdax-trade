{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Server where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Routes

import           Control.Monad.Reader
import qualified Data.ByteString.Char8           as C
import           Data.Maybe
import           Data.String.Conversions
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.HttpAuth

server :: ReaderT Config IO ()
server = do
  conf <- ask
  sc@ServerConf {..} <- reader serverConf
  logInfo $ "Starting server with port: " ++ show port
  let middleware = sslUpgrade . authT sc
      application = middleware $ routes conf
  liftIO $ run port application

sslUpgrade :: Middleware
sslUpgrade app req = do
  case requestHeaderHost req of
    Just host
      | C.isPrefixOf "localhost" host -> app req
    _ -> forceSSL app req

authT :: ServerConf -> Middleware
authT ServerConf {..} =
  if isNothing maybeUsername || isNothing maybePassword
    then id
    else basicAuth
           (\u p ->
              return $
              u == (cs . fromJust) maybeUsername &&
              p == (cs . fromJust) maybePassword)
           "Gdax Trade App"
