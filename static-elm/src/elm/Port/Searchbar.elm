port module Port.Searchbar exposing (..)


type alias SearchbarPortArgs =
    { dropdownId : String
    }


port searchbarPort : SearchbarPortArgs -> Cmd msg
