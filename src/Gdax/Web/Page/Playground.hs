{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Gdax.Web.Page.Playground where

import           Gdax.Util.Config
import           Gdax.Util.Network
import           Gdax.Web.Template

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
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
import           System.Log.Logger
import           Text.Blaze.Html5            (dataAttribute, string,
                                              stringValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

title = "Playground"

playground :: ServerPart Response
playground = ok $ toResponse $ render Nothing

playgroundSearch :: Config -> ServerPart Response
playgroundSearch config = do
    liftIO $ debugM title "Handling search."
    searchMethod <- look "search_method"
    searchUrl <- look "search_url"
    liftIO $ debugM title $ "Received search params: " ++ searchMethod ++ ", " ++ searchUrl
    res <- liftIO $ runReaderT (gdaxRequest searchMethod searchUrl) config
    ok $ toResponse $ render $ Just res

render :: Maybe (Either SomeException ByteString) -> H.Html
render content =
    template title $ do
        userInputForm
        when (isJust content) $ outputArea (fromJust content)
        gdaxDocs
        floatingButtons

userInputForm :: H.Html
userInputForm =
    H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/playground/search" $
    H.div ! A.class_ "flex" $ do
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label getmdl-select" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.id "search_method" ! A.name "search_method" !
                A.value "GET" !
                A.readonly "true" !
                A.tabindex "-1"
            H.label ! A.for "search_method" $ do
                H.i ! A.class_ "mdl-icon-toggle__label material-icons" $ "keyboard_arrow_down"
            H.label ! A.for "search_method" ! A.class_ "mdl-textfield__label" $ "Select method"
            H.ul ! A.for "search_method" ! A.class_ "mdl-menu mdl-menu--bottom-left mdl-js-menu" $ do
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "GET" $ "GET"
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "POST" $ "POST"
                H.li ! A.class_ "mdl-menu__item" ! dataAttribute "val" "DELETE" $ "DELETE"
        H.div ! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
            H.input ! A.class_ "mdl-textfield__input" ! A.type_ "text" ! A.name "search_url" ! A.id "url_label"
            H.label ! A.class_ "mdl-textfield__label" ! A.for "url_label" $ string "Enter url"
        H.input ! A.class_ "mdl-button mdl-js-button mdl-button--raised mdl-button--colored mdl-js-ripple-effect " ! A.type_ "submit" !
            A.value "Send"

outputArea :: Either SomeException ByteString -> H.Html
outputArea res = do
    H.div ! A.class_ "output-area demo-card-wide mdl-card mdl-shadow--2dp" $ do
        H.div ! A.class_ "mdl-card__title" $ H.h2 ! A.class_ "mdl-card__title-text" $ "Response"
        H.div ! A.class_ "mdl-card__supporting-text" $ do
            H.pre !
                A.class_
                    (stringValue $
                     "output-text" ++
                     if isLeft res
                         then " error"
                         else "") $
                string $ either show cs res

gdaxDocs :: H.Html
gdaxDocs = H.iframe ! A.class_ "gdax-docs" ! A.src "https://docs.gdax.com/" $ return ()

floatingButtons :: H.Html
floatingButtons = do
    H.button ! A.class_ "fab fab-up mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--colored" $ do
        H.i ! A.class_ "material-icons" $ "keyboard_arrow_up"
    H.button ! A.class_ "fab fab-down mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--primary" $ do
            H.i ! A.class_ "material-icons" $ "keyboard_arrow_down"
