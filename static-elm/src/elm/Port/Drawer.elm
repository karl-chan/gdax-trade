port module Port.Drawer exposing (..)


type alias DrawerPortArgs =
    { id : String
    , toggleId : String
    }


port drawerPort : DrawerPortArgs -> Cmd msg
