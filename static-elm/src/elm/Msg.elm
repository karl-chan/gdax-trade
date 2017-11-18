module Msg exposing (..)

import Model.Searchbar exposing (..)


type Msg
    = NoOp
    | OnInitHttpResponse (Result Http.Error InitHttpResponse)
    | OnSearchRequest SearchRequest
