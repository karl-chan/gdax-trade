{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Routes where

import           Gdax.Util.Config
import           Gdax.Web.Handlers.Proxy
import           Gdax.Web.Handlers.Stream

import           Network.Wai
import           Network.Wai.Application.Static

routes :: Config -> Application
routes conf req respond = do
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "rest"]) -> proxy conf req respond
    ("POST", ["api", "stream"]) -> stream conf req respond
    _ -> staticApp (defaultFileServerSettings "static") req respond
