module PlaygroundUpdate exposing (..)

import Msg exposing (..)
import Model exposing (..)
import PlaygroundMsg exposing (..)
import PlaygroundModel exposing (..)
import Config exposing (..)
import Http exposing (..)
import Json.Encode as J exposing (..)

update : Config -> PlaygroundMsg -> PlaygroundModel ( PlaygroundModel, Cmd Msg )
update config playgroundMsg playgroundModel =
    case playgroundMsg of
        Paginate isBefore sequence -> (playgroundModel, getBeforePage config playgroundModel.userInput )
        OnMethodSelect url -> (,)
        Submit searchMethod url -> (playgroundModel, submit searchMethod url)

getSearchEndpoint : Config -> String
getSearchEndpoint config = config.host + "/search"

getBeforePage : Config -> UserInput ->Cmd Msg
getBeforePage config userInput =
    let endpoint = getSearchEndpoint config
        request = Http.post endPoint


submit : SearchMethod -> Url -> Cmd Msg
submit searchMethod url =
    let url =
