{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Playground.View.Fragment.Stream where

import           Gdax.Types.Product
import           Gdax.Web.Playground.Model
import           Gdax.Web.Types

import           Data.ByteString.Lazy        (ByteString)
import           Data.String.Conversions
import           Gdax.Web.Template
import           Text.Blaze.Html5            (string, stringValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

streamFragment :: PlaygroundModel -> H.Html
streamFragment Stream {..} = render products url initialMessage
streamFragment model =
  error $ "Only stream model is supported, unsupported model: " ++ show model

render :: [Product] -> SearchUrl -> ByteString -> PlaygroundContent
render products url initialMessage =
  H.div ! A.class_ "output-card demo-card-wide mdl-card mdl-shadow--2dp" $ do
    H.div ! A.class_ "mdl-card__title output-title" $ do
      H.h2 ! A.class_ "mdl-card__title-text" $
        string $ "Response from socket with products: " ++ show products
      H.button !
        A.class_
          "stop-streaming mdl-button mdl-js-button mdl-button--raised mdl-button--colored" $
        "Stop"
    H.div ! A.class_ "mdl-card__supporting-text" $
      H.pre ! A.class_ "output-text" $ return ()
    H.input ! A.type_ "hidden" ! A.name "socket_url" ! A.value (stringValue url)
    H.input ! A.type_ "hidden" ! A.name "initial_message" !
      A.value (stringValue . cs $ initialMessage)
    H.script ! A.type_ "text/javascript" ! A.defer "" !
      A.src (stringValue $ jsFile "playground/stream.js") $
      return ()

renderError :: String -> PlaygroundContent
renderError err =
  H.div ! A.class_ "output-card demo-card-wide mdl-card mdl-shadow--2dp" $ do
    H.div ! A.class_ "mdl-card__title" $
      H.h2 ! A.class_ "mdl-card__title-text" $
      string $ "Response from socket with initial message: " ++ err
    H.div ! A.class_ "mdl-card__supporting-text" $
      H.pre ! A.class_ "output-text error" $ string $ "Invalid args: " ++ err
