module Subscriptions exposing (subscriptions)

import Model exposing (..)
import Msg exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
