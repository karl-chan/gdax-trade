{-# LANGUAGE CPP #-}

module Gdax.Web.Page.Playground.Stream where

import Gdax.Util.Config
import Gdax.Web.Page.Playground

import Control.Monad.Reader
import Happstack.Server

playgroundStream :: String -> ReaderT Config (ServerPartT IO) PlaygroundContent
playgroundStream searchArgs =
    return Stream {socketUrl = "/stream", initialMessage = searchArgs}