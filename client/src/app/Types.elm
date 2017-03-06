module Types exposing (..)

import Table
import Dict
import Http
import Navigation
import Date exposing (Date)
import Set exposing (Set)


type alias Model =
    { apiHost : String
    , goodReadsUserIdInputCurrentValue : String
    , lists : Dict.Dict String (List String)
    , books : Dict.Dict String Book
    , buildInfo : BuildInfo
    , tableState : Table.State
    }


type Msg
    = LookupWatchList String
    | UserIdInput String
    | LoadGoodreadsToReadList String (Result Http.Error (List Book))
    | ClearList String
    | SetTableState Table.State
    | UrlChange Navigation.Location


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , goodReadsUserIdInputCurrentValue = ""
    , lists = Dict.empty
    , books = Dict.empty
    , tableState = Table.initialSort "Priority"
    , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
    }


type alias Book =
    { id : String
    , title : String
    , description : String
    , url : String
    , isbn : String
    , authors : List String
    , numberOfPages : Maybe Int
    , averageRating : Float
    , published : Maybe Int
    }


type alias PriorityWeights =
    { rating : Float
    , length : Float
    }



-- BUILD INFO


type alias Flags =
    { apiHost : String
    , buildVersion : String
    , buildTier : String
    , buildTime : String
    }


type alias BuildInfo =
    { version : String
    , time : String
    , tier : String
    }
