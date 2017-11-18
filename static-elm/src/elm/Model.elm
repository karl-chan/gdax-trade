module Model exposing (..)

import Model.Drawer exposing (DrawerModel)
import Model.Navbar exposing (NavbarModel)
import Model.Searchbar exposing (SearchbarModel)


type alias Model =
    { appName : String
    , navbar : NavbarModel
    , drawer : DrawerModel
    , searchbar : SearchbarModel
    }
