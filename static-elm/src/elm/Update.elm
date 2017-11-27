module Update exposing (update)

import Focus exposing (..)
import Http exposing (..)
import Material
import Material.Helpers exposing (..)
import Material.Snackbar as Snackbar
import Model exposing (..)
import Model.Search exposing (..)
import Model.Search.SearchResult exposing (..)
import Model.Search.Searchbar exposing (..)
import Msg exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            let
                ( snackbar_, effect ) =
                    Snackbar.add (Snackbar.snackbar 1 "Snackbar message #" "UNDO") model.snackbar |> map2nd (Cmd.map Snackbar)
            in
            { model
                | snackbar = snackbar_
            }
                ! []

        OnHttpOptionsResponse result ->
            case result of
                Err err ->
                    -- updateWithServerError err model
                    let
                        ( snackbar_, _ ) =
                            Snackbar.add (Snackbar.snackbar 1 "Snackbar message #" "UNDO") model.snackbar
                    in
                    { model
                        | snackbar = snackbar_
                    }
                        ! []

                Ok res ->
                    set (search => searchbar => options) (Just res) model ! []

        OnHttpSearchResponse result ->
            case result of
                Err err ->
                    -- updateWithServerError err model
                    ( model, Cmd.none )

                Ok res ->
                    set (search => searchResult => response) (Just res) model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model

        _ ->
            ( model, Cmd.none )
