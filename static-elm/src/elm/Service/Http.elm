module Service.Http exposing (..)

import Http
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Msg exposing (..)


type alias InitHttpResponse =
    { methods : List String
    , products : List String
    }



initHttp : Cmd Msg
initHttp =
    let decoder =  decode InitHttpResponse
        |> required "methods" (list string)
        |> required "products" (list string)
    Http.send OnInitHttpResponse <| Http.get decoder "/api/init-http"
