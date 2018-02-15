module Gdax.Util.Config.Server where

import           Data.UUID

data ServerConf = ServerConf
  { maybeUsername :: Maybe String
  , maybePassword :: Maybe String
  , herokuKey     :: UUID
  , port          :: Int
  } deriving (Eq, Show)
