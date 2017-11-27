module Init exposing (..)

import Material
import Material.Snackbar as Snackbar
import Model exposing (..)
import Msg exposing (..)
import Service.Http exposing (..)


init : ( Model, Cmd Msg )
init =
    model ! [ initHttpOptions ]


model : Model
model =
    let
        navbarModel =
            {}

        drawerModel =
            { id = "gdax-drawer", toggleId = "gdax-drawer-toggle" }

        searchModel =
            { searchbar = { dropdownId = "abc", userInput = Nothing, options = Nothing }, searchResult = { response = Nothing } }
    in
    { appName = "Gdax Trade App"
    , mdl =
        Material.model
    , navbar = navbarModel
    , drawer = drawerModel
    , search = searchModel
    , snackbar = Snackbar.model
    }
