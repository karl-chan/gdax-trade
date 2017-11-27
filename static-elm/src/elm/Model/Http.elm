module Model.Http exposing (..)

import Dict exposing (Dict)


-- HTTP RESPONSE JSON BODY


type alias HttpOptionsResponse =
    { restMethods : List String
    , products : List String
    }


type HttpSearchRequest
    = RestRequest HttpRestRequest
    | StreamRequest HttpStreamRequest


type alias HttpRestRequest =
    { method : String
    , endpoint : String
    , body : Dict String String
    }


type alias HttpStreamRequest =
    { products : List String
    }


type HttpSearchResponse
    = RestResponse HttpRestResponse
    | StreamResponse HttpStreamResponse


type alias HttpRestResponse =
    { isError : Bool
    , method : String
    , endpoint : String
    , body : String
    , before : Maybe String
    , after : Maybe String
    }


type alias HttpStreamResponse =
    { isError : Bool
    , products : List String
    , body : String
    }
