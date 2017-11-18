module View.Searchbar exposing (searchbar)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Model exposing (..)
import Msg exposing (..)


searchbar : Model -> Html Msg
searchbar model =
    div [ class "row" ]
        [ div [ class "s12 m4" ]
            [ dropdown model
            ]
        , div [ class "s12 m6" ] []
        , div [ class "s12 m2" ] []
        ]


dropdown : Model -> Html Msg
dropdown model =
    let
        renderOption searchOption =
            option [ value searchOption ] [ text searchOption ]
    in
    div [ class "input-field col s12" ]
        [ select [] <|
            [ option [ value "", disabled True, selected True ]
                [ text "Select method" ]
            ]
                ++ (case model.searchbar.options of
                        Nothing ->
                            []

                        Just opt ->
                            List.map renderOption opt.methods
                   )
        , label []
            [ text "Materialize Select" ]
        ]
