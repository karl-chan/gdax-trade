{-# LANGUAGE OverloadedStrings #-}

module Gdax.Web.Server where

import           Gdax.Util.Config
import           Gdax.Web.Routes

import           Control.Monad.Reader
import qualified Data.Map                    as Map
import           Happstack.Server

server :: ReaderT Config IO ()
server = do
    config <- ask
    serverConfig <- reader serverConf
    user <- reader serverUser
    password <- reader serverPassword
    liftIO $
        simpleHTTP serverConfig $
        basicAuth "My server" (Map.fromList [(user, password)]) $ do
            decodeBody $ defaultBodyPolicy "/tmp" 0 1000 1000
            msum $ runReader routes config
