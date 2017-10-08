module Gdax.Web.Types where

import           Gdax.Util.Config

import           Control.Monad.Reader
import           Happstack.Server

type Handler = ReaderT Config (ServerPartT IO) Response

data SearchMethod
    = GET
    | POST
    | DELETE
    | STREAM
    deriving (Read, Show)