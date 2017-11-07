module Gdax.Web.Routes where

import           Gdax.Util.Config
import           Gdax.Web.Playground.Handler
import           Gdax.Web.Template

import           Control.Monad.Reader
import           Happstack.Server

routes :: Reader Config [ServerPartT IO Response]
routes = do
    conf <- ask
    return
        [ dirs "playground" $ runReaderT playgroundHandler conf
        , dir "static" $ serveDirectory EnableBrowsing [] staticDir
        , seeOther ("/playground" :: String) $ toResponse ("Redirecting to /playground..." :: String)
        ]
