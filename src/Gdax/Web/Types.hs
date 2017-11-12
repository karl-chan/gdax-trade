module Gdax.Web.Types where

import           Gdax.Util.Config

import           Control.Monad.Reader
import           Happstack.Server
import qualified Text.Blaze.Html5     as H

type Handler = ReaderT Config (ServerPartT IO) Response

data SearchMethod
  = GET
  | POST
  | DELETE
  | STREAM
  deriving (Read, Show)

type SearchUrl = String

type SearchArgs = String

type Before = String

type After = String

data PaginationOptions = PaginationOptions
  { before :: Maybe Before
  , after  :: Maybe After
  } deriving (Eq, Show)

type PlaygroundContent = H.Html
