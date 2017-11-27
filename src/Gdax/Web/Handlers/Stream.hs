{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.Stream where

import           Gdax.Util.Config
import           Gdax.Util.Logger

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Socket

import           Data.Aeson
import qualified Data.Map                       as Map
import           Data.String.Conversions
import           GHC.Generics
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai

newtype StreamResponse = Ok
  { auth :: Auth
  } deriving (Generic, ToJSON)

stream :: Config -> Application
stream Config {..} _ respond = do
  auth <- mkAuth exchangeConf
  respond $
    responseLBS status200 [(hContentType, "application/json")] $
    encode $ Ok auth
