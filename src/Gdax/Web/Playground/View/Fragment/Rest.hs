{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Web.Playground.View.Fragment.Rest where

import           Gdax.Web.Playground.Model
import           Gdax.Web.Template
import           Gdax.Web.Types

import           Control.Monad.Reader
import           Data.ByteString.Lazy        (ByteString)
import           Data.String.Conversions
import           Prelude                     hiding (seq)
import           Text.Blaze.Html5            (string, stringValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

simpleFragment :: PlaygroundModel -> H.Html
simpleFragment Rest {..} =
  render method url pagination responseBody timeTakenMillis
simpleFragment model =
  error $ "Only rest model is supported, unsupported model: " ++ show model

render ::
     SearchMethod
  -> SearchUrl
  -> PaginationOptions
  -> ByteString
  -> Double
  -> H.Html
render method url pagination textContent timeTakenMillis =
  H.div ! A.class_ "output-card demo-card-wide mdl-card mdl-shadow--2dp" $ do
    H.div ! A.class_ "mdl-card__title output-title" $ do
      H.h2 ! A.class_ "mdl-card__title-text" $ do
        "Response from"
        H.span ! A.class_ "output-url mdl-color-text--primary" $ string url
      H.span ! A.class_ "time_taken" $
        string $ "Took " ++ show timeTakenMillis ++ " ms."
      renderPagination method url pagination
    H.div ! A.class_ "mdl-card__supporting-text" $
      H.pre ! A.class_ "output-text" $ return ()
    H.input ! A.type_ "hidden" ! A.name "response_text" !
      A.value (stringValue . cs $ textContent)
    H.script ! A.type_ "text/javascript" ! A.defer "" !
      A.src (stringValue $ jsFile "playground/search.js") $
      return ()

renderPagination :: SearchMethod -> SearchUrl -> PaginationOptions -> H.Html
renderPagination method url pagination =
  H.form ! A.class_ "pagination_form" ! A.enctype "multipart/form-data" !
  A.method "POST" !
  A.action "/playground" $ do
    H.input ! A.type_ "hidden" ! A.name "search_method" !
      A.value (stringValue . show $ method)
    H.input ! A.type_ "hidden" ! A.name "search_args" !
      A.value (stringValue url)
    H.div ! A.class_ "row pagination" $
            -- note that before is newer, after is older
     do
      forM_ (after pagination) $ \seq -> do
        H.div ! A.class_ "mdl-color-text--accent" $ string $ "Prev " ++ seq
        H.button !
          A.class_
            "mdl-button mdl-js-button mdl-button--icon mdl-button--accent" !
          A.type_ "submit" !
          A.name "after" !
          A.value (stringValue seq) $
          H.i ! A.class_ "material-icons" $ "chevron_left"
      forM_ (before pagination) $ \seq -> do
        H.button !
          A.class_
            "mdl-button mdl-js-button mdl-button--icon mdl-button--primary" !
          A.type_ "submit" !
          A.name "before" !
          A.value (stringValue seq) $
          H.i ! A.class_ "material-icons" $ "chevron_right"
        H.div ! A.class_ "mdl-color-text--primary" $ string $ "Next " ++ seq
