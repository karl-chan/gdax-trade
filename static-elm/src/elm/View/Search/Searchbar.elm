module View.Search.Searchbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Model exposing (..)
import Msg exposing (..)


searchbar : Model -> Html Msg
searchbar model =
    div [] []



-- []
-- div
-- [ class "row" ]
-- [ div [ class "s12 m4" ]
--     [ dropdown model
--     ]
-- , div [ class "s12 m6" ] []
-- , div [ class "s12 m2" ] []
-- ]


dropdown : Model -> Html Msg
dropdown model =
    -- let
    --     renderOption searchOption =
    --         option [ value searchOption ] [ text searchOption ]
    --     options =
    --         case model.search.options of
    --             Just (Ok opts) ->
    --                 List.map renderOption opts.methods
    --             _ ->
    --                 []
    -- in
    -- div [ class "input-field col s12" ]
    --     [ select [ onInput SetSearchMethod ] <|
    --         [ option [ value "", disabled True, selected True ]
    --             [ text "Select method" ]
    --         ]
    --             ++ options
    --     , label []
    --         [ text "Materialize Select" ]
    --     ]
    div [] []
