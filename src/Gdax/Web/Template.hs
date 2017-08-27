{-# LANGUAGE OverloadedStrings #-}

module Gdax.Web.Template where

import           Control.Monad
import           Happstack.Server
import           System.FilePath.Posix

import           Data.List
import           System.Directory
import           System.IO.Unsafe
import           Text.Blaze.Html5            as H hiding (map)
import           Text.Blaze.Html5.Attributes as A

type Async = Bool

template :: String -> H.Html -> H.Html
template title body =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
            forM_ cssPaths $ \path -> H.link ! A.rel "stylesheet" ! A.href (stringValue path)
        H.body $ do
            body
            forM_ jsPaths $ \(path, async) ->
                if async
                    then H.script ! A.type_ "text/javascript" ! A.defer "" ! A.src (stringValue path) $ return ()
                    else H.script ! A.type_ "text/javascript" ! A.src (stringValue path) $ return ()

staticDir :: FilePath
staticDir = "static"

cssPaths :: [FilePath]
cssPaths =
    [ "https://fonts.googleapis.com/icon?family=Material+Icons"
    , "https://code.getmdl.io/1.3.0/material.indigo-pink.min.css"
    , "https://cdn.rawgit.com/CreativeIT/getmdl-select/master/getmdl-select.min.css"
    , "http://fonts.googleapis.com/css?family=Roboto:300,400,500,700"
    ] ++
    let cssDir = staticDir </> "css"
        cssFiles = filter (isSuffixOf ".css") $ (unsafePerformIO . getDirectoryContents) cssDir
    in map (("/" </> cssDir) </>) cssFiles

jsPaths :: [(FilePath, Async)]
jsPaths =
    [ ("https://code.getmdl.io/1.3.0/material.min.js", True)
    , ("https://cdn.rawgit.com/CreativeIT/getmdl-select/master/getmdl-select.min.js", True)
    , ("https://code.jquery.com/jquery-3.2.1.min.js", False)
    ] ++
    let jsDir = staticDir </> "js"
        jsFiles = filter (isSuffixOf ".js") $ (unsafePerformIO . getDirectoryContents) jsDir
    in map (\filename -> ("/" </> jsDir </> filename, False)) jsFiles
