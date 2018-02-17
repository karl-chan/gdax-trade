{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Routes where

import           Gdax.Util.Config
import           Gdax.Web.Handlers.Heroku
import           Gdax.Web.Handlers.Rest
import           Gdax.Web.Handlers.Stream

import           Network.Wai
import           Network.Wai.Application.Static

routes :: Config -> Application
routes conf req respond = do
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "rest"]) -> restHandler conf req respond
    ("POST", ["api", "stream"]) -> streamHandler conf req respond
    ("POST", ["heroku", "start"]) -> herokuHandler Start req respond
    ("POST", ["heroku", "stop"]) -> herokuHandler Stop req respond
    ("POST", ["heroku", "restart"]) -> herokuHandler Restart req respond
    _ -> staticApp (defaultFileServerSettings "static") req respond
