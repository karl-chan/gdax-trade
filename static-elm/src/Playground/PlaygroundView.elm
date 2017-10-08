module PlaygroundView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (..)
import PlaygroundModel exposing (..)
import PlaygroundMsg exposing (..)


-- VIEW --


playgroundView : PlaygroundModel -> Html Msg
playgroundView playgroundModel =
    div []
        [ renderUserInputRow playgroundModel
        , renderContent playgroundModel
        , renderPagination playgroundModel
        , renderGdaxDocs
        , renderFABs
        ]


renderUserInputRow : PlaygroundModel -> Html Msg
renderUserInputRow playgroundModel =
    form
        [ onSubmit PlaygroundMsg Submit
        ]
        [ div
            [ class "row search_bar" ]
            [ div
                [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label getmdl-select" ]
                [ input
                    [ class "mdl-textfield__input"
                    , type_ "text"
                    , id "search_method"
                    , value "GET"
                    , readonly True
                    , tabindex "-1"
                    , onInput PlaygroundMsg OnMethodSelect
                    ]
                    []
                , label
                    [ for "search_method"
                    , class "mdl-icon-toggle__label material-icons"
                    ]
                    [ text "keyboard_arrow_down" ]
                , label
                    [ for "search_method"
                    , class "mdl-textfield__label"
                    ]
                    [ text "Select method" ]
                , ul
                    [ for "search_method"
                    , class "mdl-menu mdl-menu--bottom-left mdl-js-menu"
                    ]
                    [ li [ class "mdl-menu__item", attribute "data-val" "GET" ]
                        [ text "GET" ]
                        li
                        [ class "mdl-menu__item", attribute "data-val" "POST" ]
                        [ text "POST" ]
                        li
                        [ class "mdl-menu__item", attribute "data-val" "DELETE" ]
                        [ text "DELETE" ]
                        li
                        [ class "mdl-menu__item", attribute "data-val" "STREAM" ]
                        [ text "STREAM" ]
                    ]
                ]
            , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
                [ input [ class "mdl-textfield__input", type_ "text", name "search_args", id "url_label" ] []
                , label [ class "mdl-textfield__label", for "url_label" ] [ text "Enter url" ]
                , button
                    [ class "mdl-button mdl-js-button mdl-button--raised mdl-button--colored mdl-js-ripple-effect "
                    , type_ "Submit"
                    ]
                    []
                ]
            ]
        ]


renderContent : PlaygroundModel -> Html Msg
renderContent playgroundModel =
    div [ class "output-area demo-card-wide mdl-card mdl-shadow--2dp" ]
        [ div [ class "mdl-card__title" ]
            [ h2 [ class "mdl-card__title-text" ]
                [ text "Response from"
                , span [ class "output-url mdl-color-text--primary" ]
                    [ text playgroundModel.url ]
                ]
            ]
        , case playgroundModel.pagination of
            Nothing ->
                text ""

            Maybe pagination ->
                renderPagination pagination
        , div [ class "mdl-card__supporting-text" ]
            [ pre [ classList [ ( "output-text", True ), ( "error", playgroundModel.error ) ] ]
                [ text content ]
            ]
        ]


renderPagination : Pagination -> Html Msg
renderPagination pagination =
    -- note that before is newer, after is older
    div [ class "row pagination" ]
        [ case pagination.after of
            Nothing ->
                text ""

            Maybe sequence ->
                div
                    [ div [ class "mdl-color-text--accent" ]
                        [ "Prev " ++ sequence
                        , button
                            [ class "mdl-button mdl-js-button mdl-button--icon mdl-button--accent"
                            , onClick PlaygroundMsg Paginate False sequence
                            ]
                            [ i [ class "material-icons" ] [ text "chevron_left" ]
                            ]
                        ]
                    ]
        , case pagination.before of
            Nothing ->
                text ""

            Maybe sequence ->
                div
                    [ div [ class "mdl-color-text--primary" ]
                        [ button
                            [ class "mdl-button mdl-js-button mdl-button--icon mdl-button--primary"
                            , onClick PlaygroundMsg Paginate True sequence
                            ]
                            [ i [ class "material-icons" ] [ text "chevron_right" ]
                            ]
                        , "Next " ++ sequence
                        ]
                    ]
        ]


renderGdaxDocs : Html Msg
renderGdaxDocs =
    iframe [ class "gdax-docs", src "https://docs.gdax.com/" ] []


renderFABs : Html Msg
renderFABs =
    div []
        [ button [ class "fab fab-up mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--colored" ]
            [ i [] class "material-icons"
            ]
            [ "keyboard_arrow_up" ]
        , button [ class "fab fab-down mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--primary" ]
            [ i [ class "material-icons" ] [ "keyboard_arrow_down" ]
            ]
        ]
