{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Proxy where

import           Gdax.Util.Config
import           Gdax.Web.Types

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

import           Control.Exception
import           Control.Monad.Reader
import           Data.ByteString.Lazy    (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Gdax.Util.Logger
import           Network.HTTP.Client
import           System.CPUTime

type Sandbox = Bool

data SearchResponse
    = SearchError SomeException
    | SearchResponse { body            :: ByteString
                     , pagination      :: PaginationOptions
                     , timeTakenMillis :: Double }

noPagination :: PaginationOptions
noPagination = PaginationOptions {before = Nothing, after = Nothing}

gdaxRequest :: SearchMethod -> String -> PaginationOptions -> Sandbox -> ReaderT Config IO SearchResponse
gdaxRequest method url PaginationOptions {..} sandbox = do
    conf <-
        if sandbox
            then reader sandboxExchangeConf
            else reader liveExchangeConf
    let mkParam name param = name ++ "=" ++ param
        queryParams =
            intercalate "&" $ map (mkParam "before") (maybeToList before) ++ map (mkParam "after") (maybeToList after)
        appender =
            if '?' `elem` url
                then "&"
                else "?"
        urlWithParams =
            url ++
            if null queryParams
                then ""
                else appender ++ queryParams
    logDebug $ "Making " ++ show method ++ " request to url: " ++ urlWithParams
    liftIO . handle (return . SearchError) $
        execExchange conf $ do
            startTime <- liftIO getCPUTime
            res <- coinbaseRequest (cs . show $ method) True urlWithParams voidBody
            endTime <- liftIO getCPUTime
            let timeTakenMillis = realToFrac (endTime - startTime) * 1e-9 :: Double
                headers = responseHeaders res
            body <- responseBody res $$+- sinkLbs
            let paginationResults =
                    PaginationOptions
                    {before = cs <$> lookup "CB-BEFORE" headers, after = cs <$> lookup "CB-AFTER" headers}
            return SearchResponse {pagination = paginationResults, body = body, timeTakenMillis = timeTakenMillis}
