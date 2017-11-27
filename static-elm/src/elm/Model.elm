module Model exposing (..)

import Focus exposing (..)
import Material
import Material.Snackbar as Snackbar
import Model.Drawer exposing (DrawerModel)
import Model.Navbar exposing (NavbarModel)
import Model.Search exposing (SearchModel)


type alias Model =
    { appName : String
    , mdl : Material.Model
    , navbar : NavbarModel
    , drawer : DrawerModel
    , search : SearchModel
    , snackbar : Snackbar.Model Int
    }



-- FOCI


navbar : Focus { r | navbar : a } a
navbar =
    create .navbar (\f r -> { r | navbar = f r.navbar })


drawer : Focus { r | drawer : a } a
drawer =
    create .drawer (\f r -> { r | drawer = f r.drawer })


search : Focus { r | search : a } a
search =
    create .search (\f r -> { r | search = f r.search })
