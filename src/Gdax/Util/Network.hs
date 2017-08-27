{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Util.Network where

import           Gdax.Util.Config

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

import           Control.Exception
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                  (Text)
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           System.Log.Logger

data PaginationOptions = PaginationOptions
    { before :: Maybe String
    , after  :: Maybe String
    } deriving (Eq, Show)

noPagination :: PaginationOptions
noPagination = PaginationOptions {before = Nothing, after = Nothing}

cbBefore :: HeaderName
cbBefore = "CB-BEFORE"

cbAfter :: HeaderName
cbAfter = "CB-AFTER"

gdaxRequest ::
       String -> String -> PaginationOptions -> ReaderT Config IO (Either SomeException (PaginationOptions, ByteString))
gdaxRequest method url PaginationOptions {..} = do
    conf <- reader exchangeConf
    let mkParam name param = name ++ "=" ++ param
        queryParams =
            intercalate "&" $ map (mkParam "before") (maybeToList before) ++ map (mkParam "after") (maybeToList after)
        appender =
            if '?' `elem` url
                then "&"
                else "?"
        urlWithParams = url ++ appender ++ queryParams

    liftIO $ debugM "Network.hs" $ "Making " ++ (cs method) ++ " request to url: " ++ urlWithParams
    liftIO . try $
        execExchange conf $ do
            res <- coinbaseRequest (cs method) True urlWithParams voidBody
            let headers = responseHeaders res
            body <- responseBody res $$+- sinkLbs
            let paginationResults =
                    PaginationOptions
                    {before = fmap cs $ lookup cbBefore headers, after = fmap cs $ lookup cbAfter headers}
            return (paginationResults, body)
