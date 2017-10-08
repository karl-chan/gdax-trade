{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Proxy where

import           Gdax.Util.Config
import           Gdax.Web.Types

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
import           Network.HTTP.Client
import           System.Log.Logger

data Pagination = Pagination
    { before :: Maybe String
    , after  :: Maybe String
    } deriving (Eq, Show)

noPagination :: Pagination
noPagination = Pagination {before = Nothing, after = Nothing}

gdaxRequest :: SearchMethod -> String -> Pagination -> ReaderT Config IO (Either SomeException (Pagination, ByteString))
gdaxRequest method url Pagination {..} = do
    conf <- reader exchangeConf
    let mkParam name param = name ++ "=" ++ param
        queryParams =
            intercalate "&" $ map (mkParam "before") (maybeToList before) ++ map (mkParam "after") (maybeToList after)
        appender =
            if '?' `elem` url
                then "&"
                else "?"
        urlWithParams = url ++ appender ++ queryParams
    liftIO $ debugM "Network.hs" $ "Making " ++ show method ++ " request to url: " ++ urlWithParams
    liftIO . try $
        execExchange conf $ do
            res <- coinbaseRequest (cs . show $ method) True urlWithParams voidBody
            let headers = responseHeaders res
            body <- responseBody res $$+- sinkLbs
            let paginationResults =
                    Pagination {before = cs <$> lookup "CB-BEFORE" headers, after = cs <$> lookup "CB-AFTER" headers}
            return (paginationResults, body)
