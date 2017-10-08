module App exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Model exposing (..)
import Msg exposing (..)
import Update exposing (..)
import View exposing (..)


---- MODEL ----
---- UPDATE ----
---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
