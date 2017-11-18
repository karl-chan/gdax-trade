module Model.Searchbar exposing (..)


type alias SearchbarModel =
    { dropdownId : String
    , options : Maybe SearchbarOptions
    , gdaxStreamEndpoint : Maybe String
    }


type alias SearchbarOptions =
    { methods : List String
    , products : List String
    }


type SearchRequest
    = Rest RestRequest
    | Stream StreamRequest


type alias RestRequest =
    { method : String
    , endpoint : String
    }


type alias StreamRequest =
    { products : List String
    }
