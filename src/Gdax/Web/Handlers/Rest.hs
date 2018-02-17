{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Handlers.Rest where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Handlers.Error

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

import           Control.Exception         (SomeException, handle)
import           Data.Aeson                (ToJSON, encode)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Conduit              (($$+-))
import           Data.Conduit.Binary       (sinkLbs)
import qualified Data.Map                  as Map
import           Data.String.Conversions
import           Data.Text                 (Text)
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai               hiding (responseHeaders)
import           Network.Wai.Parse

type Payload = Text

data RestResponse = Ok
  { before :: Maybe Text
  , after  :: Maybe Text
  , body   :: Text
  } deriving (Generic, ToJSON)

restHandler :: Config -> Application
restHandler Config {..} req respond = do
  (params, _) <-
    parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
  let (method, endpoint, maybePayload) = decodeParams params
  res <- gdaxRequest exchangeConf method endpoint maybePayload
  case res of
    Left err ->
      let msg = cs . show $ err
      in errorHandler msg req respond
    Right (headers, body) ->
      respond $
      responseLBS status200 [(hContentType, "application/json")] $
      encode $
      Ok
      { before = cs <$> lookup "CB-BEFORE" headers
      , after = cs <$> lookup "CB-AFTER" headers
      , body = cs body
      }

gdaxRequest ::
     ExchangeConf
  -> Method
  -> String
  -> Maybe Text
  -> IO (Either SomeException (ResponseHeaders, ByteString))
gdaxRequest conf method endpoint maybePayload = do
  logDebug $ show maybePayload
  handle (\e -> return $ Left e) $
    execExchange conf $ do
      res <- coinbaseRequest method True endpoint maybePayload
      let headers = responseHeaders res
      body <- responseBody res $$+- sinkLbs
      return $ Right (headers, body)

decodeParams :: [Param] -> (Method, Endpoint, Maybe Payload)
decodeParams params =
  let paramMap = Map.fromList params
      method =
        maybe (error "Missing method in request body!") cs $
        Map.lookup "method" paramMap
      endpoint =
        maybe (error "Missing endpoint in request body!") cs $
        Map.lookup "endpoint" paramMap
      maybePayload = cs <$> Map.lookup "payload" paramMap
  in pureInfo
       ("Decoded params: " ++ show paramMap)
       (method, endpoint, maybePayload)
