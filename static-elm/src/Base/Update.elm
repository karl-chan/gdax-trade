module Update exposing (..)

import Config exposing (..)
import Model exposing (..)
import Msg exposing (..)
import PlaygroundUpdate as P exposing (..)


init : Config -> Model
init =
    { playground = P.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        config =
            model.config
    in
    case msg of
        PlaygroundMsg playgroundMsg ->
            let
                ( playgroundModel, cmd ) =
                    P.update config playgroundMsg model.playground
            in
            ( { model | playground = playgroundModel }, cmd )
