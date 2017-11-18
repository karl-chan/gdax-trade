module Main exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Port exposing (..)
import View.Drawer exposing (..)
import View.Navbar exposing (..)
import View.Searchbar exposing (..)


-- APP


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- INIT


init : ( Model, Cmd Msg )
init =
    model ! [ initPort initPortArgs, initHttp ]



-- MODEL


model : Model
model =
    let
        navbarModel =
            {}

        drawerModel =
            { id = "gdax-drawer", toggleId = "gdax-drawer-toggle" }

        searchbarModel =
            { dropdownId = "gdax-searchbar-dropdown", options = Nothing, gdaxStreamEndpoint = Nothing }
    in
    { appName = "Gdax Trade App"
    , navbar = navbarModel
    , drawer = drawerModel
    , searchbar = searchbarModel
    }



-- PORT


initPortArgs : InitPortArgs
initPortArgs =
    let
        drawerPortArgs =
            { id = model.drawer.id
            , toggleId = model.drawer.toggleId
            }

        searchbarPortArgs =
            { dropdownId = model.searchbar.dropdownId
            }
    in
    { drawer = drawerPortArgs
    , searchbar = searchbarPortArgs
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        OnSearchRequest searchRequest ->
            model ! []



--  VIEW


view : Model -> Html Msg
view model =
    div [] [ navbar model, drawer model, searchbar model ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
