{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Web.Page.Playground where

import           Gdax.Util.Config
import           Gdax.Util.Network
import           Gdax.Web.Template

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString.Char8       as C
import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.Either
import           Data.Maybe
import           Data.String                 (fromString)
import           Data.String.Conversions
import           Happstack.Server
import           Network.HTTP.Types
import           System.Log.Logger
import           Text.Blaze.Html5            (dataAttribute, string,
                                              stringValue, text, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

title = "Playground"

data PlaygroundContent = PlaygroundContent
    { isError      :: Bool
    , searchMethod :: String
    , searchUrl    :: String
    , pagination   :: PaginationOptions
    , content      :: ByteString
    }

playground :: ServerPart Response
playground = ok $ toResponse $ render Nothing

playgroundSearch :: Config -> ServerPart Response
playgroundSearch config = do
    liftIO $ debugM "Playground.hs" "Handling search."
    searchMethod <- look "search_method"
    searchUrl <- look "search_url"
    before <- optional $ look "before"
    after <- optional $ look "after"
    liftIO $ debugM "Playground.hs" $ "Received search params: " ++ searchMethod ++ ", " ++ searchUrl
    res <- liftIO $ runReaderT (gdaxRequest searchMethod searchUrl $ PaginationOptions before after) config
    ok $
        toResponse $
        render $
        Just
            PlaygroundContent
            { isError = isLeft res
            , searchMethod = searchMethod
            , searchUrl = searchUrl
            , pagination = either (const noPagination) fst res
            , content = either (cs . show) snd res
            }

-- Render --
render :: Maybe PlaygroundContent -> H.Html
render content =
    template title $ do
        renderUserInputRow
        forM_ content renderContent
        renderGdaxDocs
        renderFABs

renderUserInputRow :: H.Html
renderUserInputRow =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground/search" $
    H.div ! A.class_ "row search_bar" $ do
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label getmdl-select" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.id "search_method" ! A.name "search_method" !
                A.value "GET" !
                A.readonly "" !
                A.tabindex "-1"
            H.label ! A.for "search_method" $
                H.i ! A.class_ "mdl-icon-toggle__label material-icons" $ "keyboard_arrow_down"
            H.label ! A.for "search_method" ! A.class_ "mdl-textfield__label" $ "Select method"
            H.ul ! A.for "search_method" ! A.class_ "mdl-menu mdl-menu--bottom-left mdl-js-menu" $ do
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "GET" $ "GET"
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "POST" $ "POST"
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "DELETE" $ "DELETE"
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.name "search_url" ! A.id "url_label"
            H.label ! A.class_ "mdl-textfield__label" ! A.for "url_label" $ string "Enter url"
        H.input ! A.class_ "mdl-button mdl-js-button mdl-button--raised mdl-button--colored mdl-js-ripple-effect " !
            A.type_ "submit" !
            A.value "Send"

renderContent :: PlaygroundContent -> H.Html
renderContent pc@PlaygroundContent {..} =
    H.div ! A.class_ "output-area demo-card-wide mdl-card mdl-shadow--2dp" $ do
        H.div ! A.class_ "mdl-card__title" $
            H.h2 ! A.class_ "mdl-card__title-text" $ do
                "Response from"
                H.span ! A.class_ "output-url mdl-color-text--primary" $ string searchUrl
        when (pagination /= noPagination) $ renderPagination pc
        H.div ! A.class_ "mdl-card__supporting-text" $
            H.pre !
            A.class_
                (stringValue $
                 "output-text" ++
                 if isError
                     then " error"
                     else "") $
            (text . cs) content

renderPagination :: PlaygroundContent -> H.Html
renderPagination PlaygroundContent {..} =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground/search" $ do
        H.input ! A.type_ "hidden" ! A.name "search_method" ! A.value (stringValue searchMethod)
        H.input ! A.type_ "hidden" ! A.name "search_url" ! A.value (stringValue searchUrl)
        H.div ! A.class_ "row pagination" $
            -- note that before is newer, after is older
         do
            forM_ (after pagination) $ \seq -> do
                H.div ! A.class_ "mdl-color-text--accent" $ string $ "Prev " ++ seq
                H.button ! A.class_ "mdl-button mdl-js-button mdl-button--icon mdl-button--accent" ! A.type_ "submit" !
                    A.name "after" !
                    A.value (stringValue seq) $
                    H.i ! A.class_ "material-icons" $ "chevron_left"
            forM_ (before pagination) $ \seq -> do
                H.button ! A.class_ "mdl-button mdl-js-button mdl-button--icon mdl-button--primary" ! A.type_ "submit" !
                    A.name "before" !
                    A.value (stringValue seq) $
                    H.i ! A.class_ "material-icons" $ "chevron_right"
                H.div ! A.class_ "mdl-color-text--primary" $ string $ "Next " ++ seq

renderGdaxDocs :: H.Html
renderGdaxDocs = H.iframe ! A.class_ "gdax-docs" ! A.src "https://docs.gdax.com/" $ return ()

renderFABs :: H.Html
renderFABs = do
    H.button ! A.class_ "fab fab-up mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--colored" $
        H.i ! A.class_ "material-icons" $ "keyboard_arrow_up"
    H.button ! A.class_ "fab fab-down mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--primary" $
        H.i ! A.class_ "material-icons" $ "keyboard_arrow_down"
