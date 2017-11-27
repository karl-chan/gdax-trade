module Service.Http exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model exposing (..)
import Model.Http exposing (..)
import Model.Search.Searchbar exposing (..)
import Msg exposing (..)


initHttpOptions : Cmd Msg
initHttpOptions =
    let
        decoder =
            decode HttpOptionsResponse
                |> required "methods" (list string)
                |> required "products" (list string)

        requestOptions =
            { method = "OPTIONS"
            , headers = []
            , url = "api"
            , body = emptyBody
            , expect = expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
    in
    Http.send OnHttpOptionsResponse <| Http.request requestOptions


sendSearchRequest : SearchRequest -> Cmd Msg
sendSearchRequest searchRequest =
    case searchRequest of
        RestRequest restRequest ->
            -- sendRestRequest restRequest
            Cmd.none

        StreamRequest streamRequest ->
            -- sendStreamRequest StreamRequest
            Cmd.none



-- sendRestRequest : SearchRestRequest -> Cmd Msg
-- sendRestRequest restRequest =
--     let
--         decoder =
--             decode HttpRestResponse
--                 |> required "isError" bool
--                 |> required "method" string
--                 |> required "endpoint" string
--                 |> required "body" string
--                 |> optional "before" (nullable string)
--                 |> required "after" (nullable string)
--     in
--     Http.send OnHttpSearchResponse <| Http.get "http://www.google.com" decoder
