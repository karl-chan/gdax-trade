{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.Stream where

import           Gdax.Util.Config

import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types

import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

data StreamResponse = Ok
  { auth     :: Auth
  , endpoint :: Endpoint
  } deriving (Generic, ToJSON)

streamHandler :: Config -> Application
streamHandler Config {..} _ respond = do
  auth <- mkAuth exchangeConf
  respond $
    responseLBS status200 [(hContentType, "application/json")] $
    encode $ Ok {auth = auth, endpoint = "wss://" ++ liveSocket}
