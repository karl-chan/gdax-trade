{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Web.Page.Playground where

import           Gdax.Web.Proxy
import           Gdax.Web.Template
import           Gdax.Web.Types

import           Control.Monad
import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.String.Conversions
import           Happstack.Server            hiding (DELETE, GET, POST)
import           Text.Blaze.Html5            (dataAttribute, string,
                                              stringValue, text, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data PlaygroundContent
    = Static { isError      :: Bool
             , searchMethod :: SearchMethod
             , searchUrl    :: String
             , pagination   :: Pagination
             , textContent  :: ByteString }
    | Stream { socketUrl      :: String
             , initialMessage :: String }

title :: String
title = "Playground"

playground :: Maybe PlaygroundContent -> ServerPart Response
playground playgroundContent = ok $ toResponse $ render playgroundContent

-- Render --
render :: Maybe PlaygroundContent -> H.Html
render playgroundContent =
    template title $ do
        H.div ! A.id "playground" $ do
            renderUserInputRow
            forM_ playgroundContent renderContent
            renderGdaxDocs
            renderFABs

renderUserInputRow :: H.Html
renderUserInputRow =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground/search" $
    H.div ! A.class_ "row search_bar" $ do
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label getmdl-select" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.id "search_method" ! A.name "search_method" !
                A.value (stringValue $ show GET) !
                A.readonly "" !
                A.tabindex "-1"
            H.label ! A.for "search_method" $
                H.i ! A.class_ "mdl-icon-toggle__label material-icons" $ "keyboard_arrow_down"
            H.label ! A.for "search_method" ! A.class_ "mdl-textfield__label" $ "Select method"
            H.ul ! A.for "search_method" ! A.class_ "mdl-menu mdl-menu--bottom-left mdl-js-menu" $
                forM_ [GET, POST, DELETE, STREAM] $ \method ->
                    H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" (stringValue . show $ method) $
                    (string . show) method
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.name "search_args" ! A.id "url_label"
            H.label ! A.class_ "mdl-textfield__label" ! A.for "url_label" $ string "Enter url"
        H.input ! A.class_ "mdl-button mdl-js-button mdl-button--raised mdl-button--colored mdl-js-ripple-effect " !
            A.type_ "submit" !
            A.value "Send"

renderContent :: PlaygroundContent -> H.Html
renderContent playgroundContent =
    H.div ! A.class_ "output-area demo-card-wide mdl-card mdl-shadow--2dp" $
    case playgroundContent of
        Static {..} -> do
            H.div ! A.class_ "mdl-card__title" $ do
                H.h2 ! A.class_ "mdl-card__title-text" $ do
                    "Response from"
                    H.span ! A.class_ "output-url mdl-color-text--primary" $ string searchUrl
                when (pagination /= noPagination) $ renderPagination playgroundContent
            H.div ! A.class_ "mdl-card__supporting-text" $
                H.pre !
                A.class_
                    (stringValue $
                     "output-text" ++
                     if isError
                         then " error"
                         else "") $
                text . cs $ textContent
        Stream {..} -> do
            H.div ! A.class_ "mdl-card__title" $
                H.h2 ! A.class_ "mdl-card__title-text" $ do
                    "Response from"
                    H.span ! A.class_ "output-url mdl-color-text--primary" $ string socketUrl
                    string $ "with initial message: " ++ initialMessage
            H.div ! A.class_ "mdl-card__supporting-text" $ H.pre ! A.class_ "output-text" $ return ()
            H.input ! A.type_ "hidden" ! A.name "initial_message" ! A.value (stringValue initialMessage)
            H.script ! A.type_ "text/javascript" ! A.defer "" ! A.src (stringValue $ jsFile "stream.js") $ return ()

renderPagination :: PlaygroundContent -> H.Html
renderPagination Static {..} =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground/search" $ do
        H.input ! A.type_ "hidden" ! A.name "searc h_method" ! A.value (stringValue . show $ searchMethod)
        H.input ! A.type_ "hidden" ! A.name "search_args" ! A.value (stringValue searchUrl)
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
