module View.Search exposing (search)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Model exposing (..)
import Msg exposing (..)
import View.Search.SearchResult exposing (..)
import View.Search.Searchbar exposing (..)


search : Model -> Html Msg
search model =
    div [] [ searchbar model, searchResult model ]
