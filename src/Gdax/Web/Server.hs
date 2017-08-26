{-# LANGUAGE OverloadedStrings #-}

module Gdax.Web.Server where

import           Gdax.Util.Config
import           Gdax.Web.Page.Playground
import           Gdax.Web.Template

import           Control.Concurrent
import           Control.Monad.Reader
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Happstack.Server
import           System.Log.Logger

logger :: String
logger = "Web.Server"

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

routes :: Reader Config [ServerPartT IO Response]
routes = do
    config <- ask
    return
        [ dirs "playground/search" $ method POST >> playgroundSearch config
        , dir "playground" playground
        , dir "static" $ serveDirectory EnableBrowsing [] staticDir
--        , seeOther ("/playground" :: Text) $ toResponse ("Redirecting to /playground..." :: Text)
        ]
