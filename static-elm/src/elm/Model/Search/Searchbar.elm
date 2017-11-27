module Model.Search.Searchbar exposing (..)

import Focus exposing (..)
import Model.Http exposing (..)


type alias SearchbarModel =
    { dropdownId : String
    , userInput : Maybe SearchUserInput
    , options : Maybe SearchbarOptions
    }


type alias SearchbarOptions =
    HttpOptionsResponse


type alias SearchUserInput =
    { rest : SearchRestRequest
    , stream : SearchStreamRequest
    }


type alias SearchRequest =
    HttpSearchRequest


type alias SearchRestRequest =
    HttpRestRequest


type alias SearchStreamRequest =
    HttpStreamRequest



-- FOCI


userInput : Focus { r | userInput : a } a
userInput =
    create .userInput (\f r -> { r | userInput = f r.userInput })


options : Focus { r | options : a } a
options =
    create .options (\f r -> { r | options = f r.options })
