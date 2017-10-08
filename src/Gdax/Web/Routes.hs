module Gdax.Web.Routes where

import           Gdax.Util.Config
import           Gdax.Web.Handler.Playground
import           Gdax.Web.Handler.Stream
import           Gdax.Web.Template

import           Control.Monad.Reader
import           Happstack.Server

routes :: Reader Config [ServerPart Response]
routes = do
    conf <- ask
    return
        [ dirs "playground" $ runReaderT playgroundHandler conf
        , dirs "stream" $ runReaderT streamHandler conf
        , dir "static/dist" $ serveDirectory EnableBrowsing [] staticDir
--        , seeOther ("/playground" :: Text) $ toResponse ("Redirecting to /playground..." :: Text)
        ]
