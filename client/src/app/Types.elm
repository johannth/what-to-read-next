module Types exposing (..)

import Table
import Dict exposing (Dict)
import Http
import Date exposing (Date)
import Navigation


type alias Model =
    { apiHost : String
    , goodReadsUserIdInputCurrentValue : String
    , shelves : Dict String (Dict String (List String))
    , read : Dict String (Dict String ReadStatus)
    , books : Dict String Book
    , errorMessage : Maybe String
    , buildInfo : BuildInfo
    , tableState : Table.State
    }


type Msg
    = LookupWatchList String
    | UserIdInput String
    | LoadGoodreadsToReadList String String (Result Http.Error ( List String, Dict String Book, Dict String ReadStatus ))
    | ClearList String
    | SetTableState Table.State
    | UrlChange Navigation.Location


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , goodReadsUserIdInputCurrentValue = ""
    , shelves = Dict.empty
    , read = Dict.empty
    , books = Dict.empty
    , errorMessage = Nothing
    , tableState = Table.initialSort "Priority"
    , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
    }


type alias Author =
    { id : String
    , name : String
    , averageRating : Float
    , ratingsCount : Int
    , textReviewsCount : Int
    }


type alias Book =
    { id : String
    , title : String
    , description : String
    , url : String
    , authors : List Author
    , numberOfPages : Maybe Int
    , averageRating : Float
    , ratingsCount : Int
    , textReviewsCount : Int
    , published : Maybe Int
    }


type alias ReadStatus =
    { startedReading : Date
    , finishedReading : Maybe Date
    }


type alias PriorityWeights =
    { rating : Float
    , authors : Float
    , secret : Float
    , passion : Float
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
