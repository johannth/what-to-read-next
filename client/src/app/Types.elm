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


meanRatingForBook : Book -> Float
meanRatingForBook book =
    case book.ratingDistribution of
        Just ratingDistribution ->
            meanRatingFromRatingDistribution ratingDistribution

        Nothing ->
            book.averageRating


ratingsCountForBook : Book -> Int
ratingsCountForBook book =
    case book.ratingDistribution of
        Just ratingDistribution ->
            ratingsCountFromRatingDistribution ratingDistribution

        Nothing ->
            book.ratingsCount


standardDeviationOfRatingsForBook : Book -> Maybe Float
standardDeviationOfRatingsForBook book =
    Maybe.map standardDeviationOfRatingsFromRatingsDistribution book.ratingDistribution


standardDeviationOfRatingsFromRatingsDistribution : RatingDistribution -> Float
standardDeviationOfRatingsFromRatingsDistribution ratingDistribution =
    sqrt (varianceOfRatingsFromRatingsDistribution ratingDistribution)


varianceOfRatingsForBook : Book -> Maybe Float
varianceOfRatingsForBook book =
    Maybe.map varianceOfRatingsFromRatingsDistribution book.ratingDistribution


varianceOfRatingsFromRatingsDistribution : RatingDistribution -> Float
varianceOfRatingsFromRatingsDistribution ratingDistribution =
    let
        mean =
            meanRatingFromRatingDistribution ratingDistribution

        n =
            ratingsCountFromRatingDistribution ratingDistribution
    in
    case n of
        0 ->
            0

        _ ->
            (toFloat ratingDistribution.rating0
                * (0.2 - mean)
                ^ 2
                + toFloat ratingDistribution.rating25
                * (0.4 - mean)
                ^ 2
                + toFloat ratingDistribution.rating50
                * (0.6 - mean)
                ^ 2
                + toFloat ratingDistribution.rating75
                * (0.8 - mean)
                ^ 2
                + toFloat ratingDistribution.rating100
                * (1 - mean)
                ^ 2
            )
                / toFloat n


type alias BetaDistributionParameters =
    { alpha : Float
    , beta : Float
    }


estimateBetaDistributionParametersForBook : Book -> Maybe BetaDistributionParameters
estimateBetaDistributionParametersForBook book =
    Maybe.map
        (\ratingDistribution ->
            let
                mean =
                    meanRatingFromRatingDistribution ratingDistribution

                variance =
                    varianceOfRatingsFromRatingsDistribution ratingDistribution
            in
            estimateBetaDistributionParameters mean variance
        )
        book.ratingDistribution


estimateBetaDistributionParameters : Float -> Float -> BetaDistributionParameters
estimateBetaDistributionParameters mean variance =
    -- mean in [0, 1]
    -- variance in []
    let
        alpha =
            ((1 - mean) / variance ^ 2 - 1 / mean) * mean ^ 2

        beta =
            alpha * (1 / mean - 1)
    in
    { alpha = alpha, beta = beta }


ratingsCountFromRatingDistribution : RatingDistribution -> Int
ratingsCountFromRatingDistribution ratingDistribution =
    ratingDistribution.rating0 + ratingDistribution.rating25 + ratingDistribution.rating50 + ratingDistribution.rating75 + ratingDistribution.rating100


meanRatingFromRatingDistribution : RatingDistribution -> Float
meanRatingFromRatingDistribution ratingDistribution =
    let
        ratingsCount =
            ratingsCountFromRatingDistribution ratingDistribution
    in
    case ratingsCount of
        0 ->
            0.0

        _ ->
            (0.2 * toFloat ratingDistribution.rating0 + 0.4 * toFloat ratingDistribution.rating25 + 0.6 * toFloat ratingDistribution.rating50 + 0.8 * toFloat ratingDistribution.rating75 + 1.0 * toFloat ratingDistribution.rating100) / toFloat ratingsCount


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
