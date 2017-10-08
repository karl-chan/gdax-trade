{-# LANGUAGE CPP #-}

module Gdax.Web.Page.Playground.Search where

import           Gdax.Util.Config
import           Gdax.Web.Page.Playground
import           Gdax.Web.Proxy
import           Gdax.Web.Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Either
import           Data.String.Conversions
import           Happstack.Server         hiding (method)
import           System.Log.Logger

playgroundSearch :: SearchMethod -> String -> ReaderT Config (ServerPartT IO) PlaygroundContent
playgroundSearch method url = do
    maybeBefore <- optional $ look "before"
    maybeAfter <- optional $ look "after"
    let beforeStr = maybe "" (" Before: " ++) maybeBefore
        afterStr = maybe "" (" After: " ++) maybeAfter
    liftIO $ debugM __FILE__ $ "Handling search request: " ++ show method ++ ", " ++ url ++ beforeStr ++ afterStr
    res <- mapReaderT liftIO $ gdaxRequest method url $ Pagination maybeBefore maybeAfter
    return
        Static
        { isError = isLeft res
        , searchMethod = method
        , searchUrl = url
        , pagination = either (const noPagination) fst res
        , textContent = either (cs . show) snd res
        }
