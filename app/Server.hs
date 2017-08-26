module Main where

import           Gdax.Util.Config
import           Gdax.Web.Server

import           Control.Monad.Reader

main :: IO ()
main = do
    config <- getGlobalConfig
    runReaderT server config
