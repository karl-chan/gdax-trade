module Gdax.Web.Handler.Playground where

import           Gdax.Web.Page.Playground
import           Gdax.Web.Page.Playground.Search
import           Gdax.Web.Page.Playground.Stream
import           Gdax.Web.Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Maybe
import           Happstack.Server

playgroundHandler :: Handler
playgroundHandler = do
    maybeSearchMethod <- optional $ look "search_method"
    maybeSearchArgs <- optional $ look "search_args"
    playgroundContent <-
        case maybeSearchMethod >>= read of
            Nothing -> return Nothing
            Just STREAM -> Just <$> (playgroundStream $ fromJust maybeSearchArgs)
            Just method -> Just <$> (playgroundSearch method $ fromJust maybeSearchArgs)
    lift $ playground playgroundContent
