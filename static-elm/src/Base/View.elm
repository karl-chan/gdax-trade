module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (..)
import PlaygroundView exposing (..)


view : Model -> Html Msg
view model =
    playgroundView model.playground
