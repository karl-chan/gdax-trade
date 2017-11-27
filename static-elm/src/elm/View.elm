module View exposing (view)

import Html exposing (..)
import Material.Color as Color
import Material.Layout as Layout
import Material.Options exposing (Style)
import Model exposing (..)
import Msg exposing (..)
import View.Drawer exposing (..)
import View.Navbar exposing (..)
import View.Search exposing (..)


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = header model
        , drawer = drawer model
        , tabs = tabs model
        , main = main2 model
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        []
        [ Layout.title [] [ text model.appName ]
        ]
    ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ text model.appName ]
    , Layout.navigation
        []
        []
    ]


tabs : Model -> ( List (Html m), List (Style m) )
tabs model =
    let
        tabTitles =
            [ "Search", "GDAX Docs" ]
    in
    ( List.map text tabTitles, [ Color.background (Color.color Color.Teal Color.S400) ] )


main2 : Model -> List (Html m)
main2 model =
    [ div [] [ text "hello world" ] ]
