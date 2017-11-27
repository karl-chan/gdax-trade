module Main exposing (..)

import Html
import Init exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Subscriptions exposing (..)
import Update exposing (..)
import View exposing (..)
import View.Drawer exposing (..)
import View.Navbar exposing (..)
import View.Search exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
