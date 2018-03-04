{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Routes where

import           Gdax.Util.Config
import           Gdax.Web.Handlers.Heroku
import           Gdax.Web.Handlers.Rest
import           Gdax.Web.Handlers.StaticData
import           Gdax.Web.Handlers.Stream

import           Network.Wai
import           Network.Wai.Application.Static
import           System.FilePath.Posix

routes :: Config -> Application
routes conf req respond = do
  case (requestMethod req, pathInfo req) of
    ("GET", ["api", "static-data", "channels"]) ->
      staticDataHandler Channel req respond
    ("GET", ["api", "static-data", "products"]) ->
      staticDataHandler Product req respond
    ("GET", ["api", "static-data", "rest-methods"]) ->
      staticDataHandler RestMethod req respond
    ("POST", ["api", "rest"]) -> restHandler conf req respond
    ("POST", ["api", "stream"]) -> streamHandler conf req respond
    ("POST", ["heroku", "start"]) -> herokuHandler Start req respond
    ("POST", ["heroku", "stop"]) -> herokuHandler Stop req respond
    ("POST", ["heroku", "restart"]) -> herokuHandler Restart req respond
    _ -> staticApp (defaultFileServerSettings webappPath) req respond

webappPath :: FilePath
webappPath = "webapp" </> "dist"
