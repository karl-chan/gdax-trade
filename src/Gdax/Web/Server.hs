{-# LANGUAGE RecordWildCards #-}

module Gdax.Web.Server where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Routes

import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Maybe
import           Happstack.Server

server :: ReaderT Config IO ()
server = do
  config <- ask
  serverConfig <- reader serverConf
  ServerCredentials {..} <- reader serverCredentials
  logInfo $ "Started server with port: " ++ (show . port $ serverConfig)
  let authT =
        if isNothing maybeUsername || isNothing maybePassword
          then id
          else basicAuth
                 "My server"
                 (Map.fromList
                    [(fromJust maybeUsername, fromJust maybePassword)])
  liftIO $
    simpleHTTP serverConfig $
    authT $ do
      decodeBody $ defaultBodyPolicy "/tmp" 0 1000 1000
      msum $ runReader routes config
