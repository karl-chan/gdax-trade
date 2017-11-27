module Model.Search exposing (..)

import Focus exposing (..)
import Model.Search.SearchResult exposing (..)
import Model.Search.Searchbar exposing (..)


type alias SearchModel =
    { searchbar : SearchbarModel
    , searchResult : SearchResultModel
    }



-- FOCI


searchbar : Focus { r | searchbar : a } a
searchbar =
    create .searchbar (\f r -> { r | searchbar = f r.searchbar })


searchResult : Focus { r | searchResult : a } a
searchResult =
    create .searchResult (\f r -> { r | searchResult = f r.searchResult })
