{-# LANGUAGE RecordWildCards #-}

module Gdax.Web.Server where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Routes

import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Happstack.Server

server :: ReaderT Config IO ()
server = do
    config <- ask
    serverConfig <- reader serverConf
    maybeServerCredentials <- reader serverCredentials
    logInfo $ "Started server with port: " ++ (show . port $ serverConfig)
    let auth =
            case maybeServerCredentials of
                Nothing -> id
                Just ServerCredentials {..} -> basicAuth "My server" (Map.fromList [(username, password)])
    liftIO $
        simpleHTTP serverConfig $
        auth $ do
            decodeBody $ defaultBodyPolicy "/tmp" 0 1000 1000
            msum $ runReader routes config
