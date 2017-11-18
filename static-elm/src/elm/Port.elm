port module Port exposing (..)

import Port.Drawer exposing (..)
import Port.Searchbar exposing (..)


type alias InitPortArgs =
    { drawer : DrawerPortArgs
    , searchbar : SearchbarPortArgs
    }


port initPort : InitPortArgs -> Cmd msg
