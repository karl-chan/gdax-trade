module PlaygroundMsg exposing (..)

import PlaygroundModel exposing (..)


type PlaygroundMsg
    = Paginate IsBefore Sequence
    | OnMethodSelect Url
    | Submit SearchMethod Url
