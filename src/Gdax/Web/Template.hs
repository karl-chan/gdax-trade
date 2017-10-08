{-# LANGUAGE OverloadedStrings #-}

module Gdax.Web.Template where

import           Control.Monad
import           System.FilePath.Posix

import           Text.Blaze.Html5            as H hiding (map)
import           Text.Blaze.Html5.Attributes as A

type Async = Bool

template :: String -> H.Html -> H.Html
template titleText bodyText =
    H.html $ do
        H.head $ do
            H.title (H.toHtml titleText)
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
            forM_ cssPaths $ \path -> H.link ! A.rel "stylesheet" ! A.href (stringValue path)
        H.body $ do
            bodyText
            forM_ jsPaths $ \path ->
                H.script ! A.type_ "text/javascript" ! A.defer "" ! A.src (stringValue path) $ return ()

staticDir :: FilePath
staticDir = "static"

cssFile :: FilePath -> FilePath
cssFile path = staticDir </> "css" </> path

jsFile :: FilePath -> FilePath
jsFile path = staticDir </> "js" </> path

cssPaths :: [FilePath]
cssPaths =
    [ "https://fonts.googleapis.com/icon?family=Material+Icons"
    , "https://code.getmdl.io/1.3.0/material.indigo-pink.min.css"
    , "https://cdn.rawgit.com/CreativeIT/getmdl-select/master/getmdl-select.min.css"
    , "http://fonts.googleapis.com/css?family=Roboto:300,400,500,700"
    , cssFile "style.css"
    , cssFile "playground.css"
    ]

jsPaths :: [FilePath]
jsPaths =
    [ "https://code.getmdl.io/1.3.0/material.min.js"
    , "https://cdn.rawgit.com/CreativeIT/getmdl-select/master/getmdl-select.min.js"
    , "https://code.jquery.com/jquery-3.2.1.min.js"
    , jsFile "scripts.js"
    , jsFile "playground.js"
    ]
