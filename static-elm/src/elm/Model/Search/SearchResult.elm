module Model.Search.SearchResult exposing (..)

import Focus exposing (..)
import Model.Http exposing (..)


type alias SearchResultModel =
    { response : Maybe HttpSearchResponse
    }



-- FOCI


response : Focus { r | response : a } a
response =
    create .response (\f r -> { r | response = f r.response })
