{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Playground.View.Fragment.Error where

import           Gdax.Web.Playground.Model
import           Gdax.Web.Types

import           Text.Blaze.Html5            (string, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

errorFragment :: PlaygroundModel -> H.Html
errorFragment Error {..} = render method args msg
errorFragment model =
  error $ "Only error model is supported, unsupported model: " ++ show model

render :: SearchMethod -> SearchArgs -> String -> H.Html
render method args msg =
  H.div ! A.class_ "output-card demo-card-wide mdl-card mdl-shadow--2dp" $ do
    H.div ! A.class_ "mdl-card__title" $
      H.h2 ! A.class_ "mdl-card__title-text" $ do
        "Response from "
        H.span ! A.class_ "output-url mdl-color-text--primary" $
          string $ show method
        " with args: "
        H.span ! A.class_ "output-url mdl-color-text--primary" $ string args
    H.div ! A.class_ "mdl-card__supporting-text" $
      H.pre ! A.class_ "output-text error" $ string msg
