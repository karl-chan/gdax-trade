module View.Search.SearchResult exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Model exposing (..)
import Msg exposing (..)


searchResult : Model -> Html Msg
searchResult model =
    div
        []
        []
