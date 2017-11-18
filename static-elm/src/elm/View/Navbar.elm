module View.Navbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)


navbar : Model -> Html Msg
navbar model =
    nav []
        [ div [ class "container" ]
            [ div
                [ class "nav-wrapper" ]
                [ div [ class "brand-logo" ]
                    [ a [ href "#!", id model.drawer.toggleId, attribute "data-activates" model.drawer.id ]
                        [ i [ class "material-icons" ]
                            [ text "menu" ]
                        ]
                    , a
                        [ href "#" ]
                        [ text model.appName ]
                    ]
                , ul [ class "right hide-on-med-and-down" ]
                    [ li
                        []
                        [ a [ href "sass.html" ]
                            [ text "Sass" ]
                        ]
                    , li []
                        [ a [ href "badges.html" ]
                            [ text "Components" ]
                        ]
                    , li []
                        [ a [ href "collapsible.html" ]
                            [ text "JavaScript" ]
                        ]
                    ]
                ]
            ]
        ]
