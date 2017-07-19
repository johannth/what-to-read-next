module Types exposing (..)

import Beta
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
    { rating1 : Int
    , rating2 : Int
    , rating3 : Int
    , rating4 : Int
    , rating5 : Int
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
            toFloat (ratingsCountFromRatingDistribution ratingDistribution)
    in
    case n of
        0 ->
            0

        _ ->
            let
                p1 =
                    toFloat ratingDistribution.rating1 / n

                p2 =
                    toFloat ratingDistribution.rating2 / n

                p3 =
                    toFloat ratingDistribution.rating3 / n

                p4 =
                    toFloat ratingDistribution.rating4 / n

                p5 =
                    toFloat ratingDistribution.rating5 / n
            in
            (p1 * ((0.2 - mean) ^ 2))
                + (p2 * ((0.4 - mean) ^ 2))
                + (p3 * ((0.6 - mean) ^ 2))
                + (p4 * ((0.8 - mean) ^ 2))
                + (p5 * ((1 - mean) ^ 2))


estimateBetaDistributionParametersForBook : Book -> Beta.BetaDistributionParameters
estimateBetaDistributionParametersForBook book =
    let
        ( mean, variance ) =
            case book.ratingDistribution of
                Just ratingDistribution ->
                    ( meanRatingFromRatingDistribution ratingDistribution, varianceOfRatingsFromRatingsDistribution ratingDistribution )

                Nothing ->
                    ( book.averageRating, 1 )
    in
    Beta.estimateBetaDistributionParameters mean variance


ratingsCountFromRatingDistribution : RatingDistribution -> Int
ratingsCountFromRatingDistribution ratingDistribution =
    ratingDistribution.rating1 + ratingDistribution.rating2 + ratingDistribution.rating3 + ratingDistribution.rating4 + ratingDistribution.rating5


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
            ((0.2 * toFloat ratingDistribution.rating1)
                + 0.4
                * toFloat ratingDistribution.rating2
                + 0.6
                * toFloat ratingDistribution.rating3
                + 0.8
                * toFloat ratingDistribution.rating4
                + 1.0
                * toFloat ratingDistribution.rating5
            )
                / toFloat ratingsCount


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
