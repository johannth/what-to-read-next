module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Navigation
import Set exposing (Set)
import Table


type alias Model =
    { apiHost : String
    , today : Maybe Date
    , goodReadsUserIdInputCurrentValue : String
    , goodReadsUserId : Maybe String
    , shelves : Dict String (List String)
    , read : Dict String ReadStatus
    , books : Dict String Book
    , errorMessage : Maybe String
    , selectedTags : Set String
    , buildInfo : BuildInfo
    , tableState : Table.State
    }


type Msg
    = LookupWatchList String
    | UserIdInput String
    | LoadGoodreadsShelf String String (Result Http.Error ( List String, Dict String Book, Dict String ReadStatus ))
    | LoadGoodreadsBookDetails (Result Http.Error (Dict String Book))
    | ClearList String
    | SetTableState Table.State
    | UrlChange Navigation.Location
    | ToggleTagFilter String
    | ReceiveDate Date


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , today = Nothing
    , goodReadsUserIdInputCurrentValue = ""
    , goodReadsUserId = Nothing
    , shelves = Dict.empty
    , read = Dict.empty
    , books = Dict.empty
    , errorMessage = Nothing
    , selectedTags = Set.empty
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
    , ratingDistribution : Maybe RatingDistribution
    , ratingsCount : Int
    , textReviewsCount : Int
    , published : Maybe Int
    , tags : Set String
    }


type alias RatingDistribution =
    { rating0 : Int
    , rating25 : Int
    , rating50 : Int
    , rating75 : Int
    , rating100 : Int
    }


averageRatingForBook : Book -> Float
averageRatingForBook book =
    case book.ratingDistribution of
        Just ratingDistribution ->
            averageFromRatingDistribution ratingDistribution

        Nothing ->
            book.averageRating


ratingsCountForBook : Book -> Int
ratingsCountForBook book =
    case book.ratingDistribution of
        Just ratingDistribution ->
            ratingsCountFromRatingDistribution ratingDistribution

        Nothing ->
            book.ratingsCount


ratingsCountFromRatingDistribution : RatingDistribution -> Int
ratingsCountFromRatingDistribution ratingDistribution =
    ratingDistribution.rating0 + ratingDistribution.rating25 + ratingDistribution.rating50 + ratingDistribution.rating75 + ratingDistribution.rating100


averageFromRatingDistribution : RatingDistribution -> Float
averageFromRatingDistribution ratingDistribution =
    let
        ratingsCount =
            ratingsCountFromRatingDistribution ratingDistribution
    in
    case ratingsCount of
        0 ->
            0.0

        _ ->
            toFloat (0 * ratingDistribution.rating0 + 25 * ratingDistribution.rating25 + 50 * ratingDistribution.rating50 + 75 * ratingDistribution.rating75 + 100 * ratingDistribution.rating100) / toFloat ratingsCount


normalizedTags : Set String -> Set String
normalizedTags tags =
    let
        fictionTags =
            Set.fromList [ "fiction", "graphic-novel" ]

        nonFictionTags =
            Set.fromList [ "non-fiction", "cooking", "cookbooks", "biography", "programming", "business", "economics", "science", "autobiographies" ]
    in
    Set.toList tags
        |> List.concatMap
            (\tag ->
                []
                    ++ (if Set.member tag fictionTags then
                            [ "fiction" ]
                        else
                            []
                       )
                    ++ (if Set.member tag nonFictionTags then
                            [ "non-fiction" ]
                        else
                            []
                       )
            )
        |> Set.fromList


type BookType
    = Fiction
    | NonFiction
    | Other


bookType : Book -> BookType
bookType book =
    let
        tags =
            normalizedTags book.tags
    in
    if Set.member "fiction" tags && Set.member "non-fiction" tags then
        Other
    else if Set.member "fiction" tags then
        Fiction
    else if Set.member "non-fiction" tags then
        NonFiction
    else
        Other


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
