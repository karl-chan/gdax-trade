module Main where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Server

import           Control.Monad.Reader

main :: IO ()
main = do
    config <- getGlobalConfig
    withGlobalLogging (logConf config) $ runReaderT server config
