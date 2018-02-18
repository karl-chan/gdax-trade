{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.StaticData where

import           Gdax.Types.Product
import           Gdax.Web.Types.Channel
import           Gdax.Web.Types.RestMethod

import           Data.Aeson
import           Data.String.Conversions
import           Data.Text                 (Text)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

data StaticDataType
  = Channel
  | Product
  | RestMethod
  deriving (Eq, Enum, Ord, Bounded)

staticDataHandler :: StaticDataType -> Application
staticDataHandler dataType _ respond = do
  respond $
    responseLBS status200 [(hContentType, "application/json")] $
    case dataType of
      Channel    -> encode allChannels
      Product    -> encode allProducts
      RestMethod -> encode $ (map cs allRestMethods :: [Text])
