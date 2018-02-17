{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.Error where

import           Data.ByteString.Lazy
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

errorHandler :: ByteString -> Application
errorHandler msg _ respond = do
  respond $ responseLBS status500 [(hContentType, "application/json")] msg
