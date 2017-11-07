{-# LANGUAGE RecordWildCards #-}

module Gdax.Web.Playground.Handler where

import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Web.Playground.Model
import           Gdax.Web.Playground.View.Template
import           Gdax.Web.Proxy
import           Gdax.Web.Types

import           Coinbase.Exchange.Socket          hiding (Error)
import           Coinbase.Exchange.Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson                        (encode)
import           Data.Char
import           Happstack.Server                  hiding (method)


playgroundHandler :: Handler
playgroundHandler = do
    maybeSearchMethod <- optional $ look "search_method"
    maybeSearchArgs <- optional $ look "search_args"
    maybeBefore <- optional $ look "before"
    maybeAfter <- optional $ look "after"
    maybeSandbox <- optional $ look "sandbox"
    let searchMethod =
            case maybeSearchMethod of
                Nothing -> Nothing
                Just m  -> Just (read m)
        sandbox =
            case maybeSandbox of
                Nothing -> False
                Just s  -> map toLower s == "true"
    model <- buildModel searchMethod maybeSearchArgs maybeBefore maybeAfter sandbox
    ok $ toResponse $ playgroundTemplate model

buildModel ::
       Maybe SearchMethod
    -> Maybe SearchArgs
    -> Maybe Before
    -> Maybe After
    -> Sandbox
    -> ReaderT Config (ServerPartT IO) PlaygroundModel
buildModel Nothing _ _ _ _ = return None
buildModel (Just STREAM) (Just args) _ _ sandbox = do
    conf <- reader exchangeConf
    auth <- liftIO $ mkAuth conf
    logDebug $ "safe read result: " ++ (show . safeReads $ args)
    case safeReads args of
        Left err -> return Error {method = STREAM, args = args, msg = err, sandboxMode = sandbox}
        Right products -> do
            let socketUrl =
                    "wss://" ++
                    if sandbox
                        then sandboxSocket
                        else liveSocket
                initialMessage = encode $ Subscribe auth $ map toId products
            return Stream {products = products, url = socketUrl, initialMessage = initialMessage, sandboxMode = sandbox}
buildModel (Just method) (Just url) maybeBefore maybeAfter sandbox = do
    let paginationRequest = PaginationOptions maybeBefore maybeAfter
    res <- mapReaderT lift $ gdaxRequest method url paginationRequest sandbox
    case res of
        SearchError err -> return Error {method = method, args = url, msg = show err, sandboxMode = sandbox}
        SearchResponse {..} ->
            return
                Rest
                { method = method
                , url = url
                , pagination = pagination
                , responseBody = body
                , sandboxMode = sandbox
                , timeTakenMillis = timeTakenMillis
                }
