module View.Drawer exposing (drawer)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)


drawer : Model -> Html Msg
drawer model =
    ul [ id model.drawer.id, class "side-nav" ]
        [ li
            []
            [ div [ class "user-view" ]
                [ div
                    [ class "background" ]
                    [ img
                        [ src "images/office.jpg" ]
                        []
                    ]
                , a [ href "#!user" ]
                    [ img [ class "circle", src "images/yuna.jpg" ]
                        []
                    ]
                , a [ href "#!name" ]
                    [ span [ class "white-text name" ]
                        [ text "John Doe" ]
                    ]
                , a [ href "#!email" ]
                    [ span [ class "white-text email" ]
                        [ text "jdandturk@gmail.com" ]
                    ]
                ]
            ]
        , li []
            [ a [ href "#!" ]
                [ i [ class "material-icons" ]
                    [ text "cloud" ]
                , text
                    "First Link With Icon"
                ]
            ]
        , li []
            [ a [ href "#!" ]
                [ text "Second Link" ]
            ]
        , li []
            [ div [ class "divider" ]
                []
            ]
        , li []
            [ a [ class "subheader" ]
                [ text "Subheader" ]
            ]
        , li []
            [ a [ class "waves-effect", href "#!" ]
                [ text "Third Link With Waves" ]
            ]
        ]
