{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.Heroku where

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

data HerokuAction
  = Start
  | Stop
  | Restart
  deriving (Eq, Enum, Ord, Bounded)

herokuHandler :: HerokuAction -> Application
herokuHandler _ _ respond = do
  respond $
    responseLBS status200 [(hContentType, "application/json")] $ "Success"
