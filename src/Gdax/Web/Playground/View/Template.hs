{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Web.Playground.View.Template where

import           Gdax.Web.Playground.Model
import           Gdax.Web.Playground.View.Fragment.Error
import           Gdax.Web.Playground.View.Fragment.Rest
import           Gdax.Web.Playground.View.Fragment.Stream
import           Gdax.Web.Template
import           Gdax.Web.Types

import           Control.Monad
import           Prelude                                  hiding (null)
import           Text.Blaze.Html5                         (dataAttribute,
                                                           string, stringValue,
                                                           (!), (!?))
import qualified Text.Blaze.Html5                         as H
import qualified Text.Blaze.Html5.Attributes              as A

playgroundTemplate :: PlaygroundModel -> H.Html
playgroundTemplate model =
    let title = "Playground"
    in template title (renderMainContent model)

renderMainContent :: PlaygroundModel -> H.Html
renderMainContent model =
    H.div ! A.id "playground" $ do
        renderUserInputRow model
        unless (null model) renderTabs
        renderFragment model
        renderGdaxDocs model
        renderFABs

renderUserInputRow :: PlaygroundModel -> H.Html
renderUserInputRow model =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground" $
    H.div ! A.class_ "row search_bar" $ do
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label getmdl-select" $ do
            H.input ! A.type_ "text" ! A.class_ "mdl-textfield__input" ! A.id "search_method" ! A.name "search_method" !
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
        H.div ! A.class_ "sandbox-toggle-container" $ do
            H.span ! A.class_ "mdl-color-text--primary" $ "Sandbox Mode"
            H.label ! A.class_ "mdl-switch mdl-js-switch mdl-js-ripple-effect" ! A.for "sandbox_toggle" $ do
                H.input ! A.type_ "checkbox" ! A.name "sandbox" ! A.value "true" ! A.id "sandbox_toggle" !
                    A.class_ "mdl-switch__input" !?
                    (isSandbox model, A.checked "")
                H.span ! A.class_ "mdl-switch__label" $ return ()

renderTabs :: H.Html
renderTabs =
    H.div ! A.class_ "tabs" $ do
        H.button ! A.class_ "tab-results mdl-button mdl-js-button mdl-button--raised mdl-button--colored" $ "Results"
        H.button ! A.class_ "tab-docs mdl-button mdl-js-button mdl-button--raised mdl-button--accent" $ "Docs"
        H.script ! A.type_ "text/javascript" ! A.defer "" ! A.src (stringValue $ jsFile "playground/tabs.js") $
            return ()

renderFragment :: PlaygroundModel -> H.Html
renderFragment model =
    H.div ! A.class_ "results" $
    case model of
        Rest {}   -> simpleFragment model
        Stream {} -> streamFragment model
        Error {}  -> errorFragment model
        _         -> return ()

renderGdaxDocs :: PlaygroundModel -> H.Html
renderGdaxDocs model = do
    let isHidden = not . null $ model
    H.iframe ! A.class_ "docs" !? (isHidden, A.style "display: none;") ! A.src "https://docs.gdax.com/" $ return ()

renderFABs :: H.Html
renderFABs =
    H.button ! A.class_ "fab fab-up mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--colored" $
    H.i ! A.class_ "material-icons" $ "keyboard_arrow_up"
