module Msg exposing (..)

import Http
import Material
import Model.Http exposing (..)


type Msg
    = NoOp
    | OnHttpOptionsResponse (Result Http.Error HttpOptionsResponse)
    | OnHttpSearchResponse (Result Http.Error HttpSearchResponse)
    | SetSearchRestMethod String
    | SetSearchPayload
    | Mdl (Material.Msg Msg)
    | OnSnackbarAction
