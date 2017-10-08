module Model exposing (..)

import Config exposing (..)
import PlaygroundModel as P exposing (..)


type alias Model =
    { config : Config
    , playground : PlaygroundModel
    }
