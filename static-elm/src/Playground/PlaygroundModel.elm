module PlaygroundModel exposing (..)

import String exposing (..)


type alias PlaygroundModel =
    { error : Maybe String
    , method : SearchMethod
    , url : Url
    , pagination : Maybe Pagination
    , userInput : UserInput
    }


type alias Pagination =
    { before : Maybe String
    , after : Maybe String
    }


type alias UserInput =
    { method : SearchMethod
    , url : String
    }


type alias SearchMethod =
    String


type alias IsBefore =
    Bool


type alias Sequence =
    String


type alias Url =
    String


init : PlaygroundModel
init =
    { error = Nothing
    , method = "GET"
    , url = ""
    , pagination = Nothing
    , userInput =
        { method = "GET"
        , url = ""
        }
    }
