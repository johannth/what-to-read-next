module State exposing (..)

import Api
import Beta
import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Navigation
import Set
import Statistics
import Task
import Time
import Types exposing (..)
import UrlParser exposing ((<?>))
import Utils


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        userIdFromPath =
            parseGoodReadsUserIdFromPath location

        initialModel =
            emptyModel flags

        initialCommands =
            [ Task.perform ReceiveDate Date.now ]
                ++ Maybe.withDefault
                    []
                    (Maybe.map (Api.fetchUserData initialModel.apiHost) userIdFromPath)
    in
    { initialModel | goodReadsUserId = userIdFromPath } ! initialCommands


parseGoodReadsUserIdFromPath : Navigation.Location -> Maybe String
parseGoodReadsUserIdFromPath location =
    let
        pathParser =
            UrlParser.oneOf [ UrlParser.s "", UrlParser.s "what-to-read-next" ]
    in
    UrlParser.parsePath (pathParser <?> UrlParser.stringParam "goodReadsUserId") location
        |> Maybe.andThen identity


updatedUrl : Model -> String
updatedUrl model =
    case model.goodReadsUserId of
        Just goodReadsUserId ->
            "?goodReadsUserId=" ++ goodReadsUserId

        _ ->
            "?"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserIdInput partialUserId ->
            { model | goodReadsUserIdInputCurrentValue = partialUserId } ! []

        LookupWatchList goodReadsUserId ->
            let
                newModel =
                    { model
                        | goodReadsUserIdInputCurrentValue = ""
                        , goodReadsUserId = Just goodReadsUserId
                        , shelves = Dict.empty
                    }
            in
            newModel
                ! (Api.fetchUserData model.apiHost goodReadsUserId
                    ++ [ Navigation.modifyUrl (updatedUrl newModel) ]
                  )

        ClearList goodReadsUserId ->
            let
                newModel =
                    { model | goodReadsUserId = Nothing, shelves = Dict.empty, read = Dict.empty }
            in
            newModel ! [ Navigation.modifyUrl (updatedUrl newModel) ]

        LoadGoodreadsShelf goodReadsUserId shelf (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.BadUrl url ->
                            "Bad url" ++ url

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "Network Error"

                        Http.BadStatus response ->
                            response.body

                        Http.BadPayload desc response ->
                            desc
            in
            { model | errorMessage = Just errorMessage } ! []

        LoadGoodreadsShelf goodReadsUserId shelf (Ok ( list, books, readStatuses )) ->
            let
                batchesOfBookIds =
                    Utils.batches 15 (Dict.values books |> List.map .id)
            in
            { model
                | shelves = Dict.insert shelf list model.shelves
                , books = Dict.union books model.books
                , read = Dict.union readStatuses model.read
            }
                ! List.map (Api.getGoodreadsBookDetails model.apiHost) batchesOfBookIds

        LoadGoodreadsBookDetails (Err error) ->
            model ! []

        LoadGoodreadsBookDetails (Ok books) ->
            let
                ratings =
                    Dict.map (\bookId book -> calculateRatingsForBook book) books
            in
            { model
                | books = Dict.union books model.books
                , ratings = Dict.union ratings model.ratings
            }
                ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        UrlChange newLocation ->
            model ! []

        ToggleTagFilter tag ->
            (if Set.member tag model.selectedTags then
                { model | selectedTags = Set.remove tag model.selectedTags }
             else
                { model | selectedTags = Set.insert tag model.selectedTags }
            )
                ! []

        ReceiveDate date ->
            { model | today = Just date } ! []


weightsForBook : Book -> PriorityWeights
weightsForBook book =
    case bookType book of
        Fiction ->
            defaultPriorityWeightsForFiction

        _ ->
            defaultPriorityWeightsForNonFiction


type PriorityType
    = WorstCase
    | AverageCase
    | BestCase


calculatePriority : PriorityType -> ( Book, CachedRating ) -> Float
calculatePriority priorityType ( book, cachedRating ) =
    let
        bookRating =
            case priorityType of
                WorstCase ->
                    cachedRating.worstCaseRating

                AverageCase ->
                    cachedRating.meanRating

                BestCase ->
                    cachedRating.bestCaseRating
    in
    Utils.interpolation (priorityWeightsToList (weightsForBook book)) (calculatePriorityValues book cachedRating.betaParameters bookRating)


calculatePriorityValues : Book -> Beta.BetaDistributionParameters -> Float -> List Float
calculatePriorityValues book betaParameters bookRating =
    let
        agreement =
            abs (betaParameters.alpha - betaParameters.beta)

        authorsAverageRating =
            calculateAuthorsAverageRating book.authors

        secretRating =
            calculateSecretRating book

        passionRating =
            calculatePassion book

        bookLengthRating =
            calculateBookLengthRating book.numberOfPages
    in
    [ bookRatingConfidence book * bookRating
    , authorsAverageRating
    , secretRating
    , passionRating
    , bookLengthRating
    ]


bookRatingConfidence : Book -> Float
bookRatingConfidence book =
    let
        ratingsCount =
            toFloat (ratingsCountForBook book)
    in
    case ratingsCount of
        0 ->
            0

        _ ->
            0.2 + 0.8 * min 1 (ratingsCount / 15)


calculateSecretRating : Book -> Float
calculateSecretRating book =
    let
        popularityRating =
            calculatePopularity (ratingsCountForBook book)
    in
    1 - popularityRating


calculateBookLengthRating : Maybe Int -> Float
calculateBookLengthRating maybeNumberOfPages =
    let
        optimalBookLengthInPages : Float
        optimalBookLengthInPages =
            300

        numberOfPages =
            Maybe.withDefault 400 (Maybe.map toFloat maybeNumberOfPages)
    in
    normalizeBookLengthWithParameters optimalBookLengthInPages 0.75 numberOfPages


normalizeBookLengthWithParameters : Float -> Float -> Float -> Float
normalizeBookLengthWithParameters optimalBookLengthInPages optimalBookLengthScore bookLengthInPages =
    let
        k =
            (optimalBookLengthInPages ^ 2 * optimalBookLengthScore) / (1 - optimalBookLengthScore)
    in
    k / (bookLengthInPages ^ 2 + k)


calculateAuthorsAverageRating : List Author -> Float
calculateAuthorsAverageRating authors =
    let
        authorsCount =
            List.length authors

        averageRating =
            Utils.interpolation (List.repeat authorsCount (1 / toFloat authorsCount)) (List.map .averageRating authors)
    in
    averageRating


calculatePopularity : Int -> Float
calculatePopularity ratingsCount =
    let
        averageRatingsCount =
            15000

        ratingForAverage =
            0.5

        f =
            Utils.increasingFunction averageRatingsCount ratingForAverage
    in
    f (toFloat ratingsCount)



-- By passion we're trying to capture effort put into book (now by looking at percentage of text reviews)


calculatePassion : Book -> Float
calculatePassion book =
    case ratingsCountForBook book of
        0 ->
            0

        ratingsCount ->
            toFloat book.textReviewsCount / toFloat ratingsCount


calculateDisagreement : Book -> Float
calculateDisagreement book =
    case book.ratingDistribution of
        Just ratingDistribution ->
            standardDeviationOfRatingsFromRatingsDistribution ratingDistribution

        Nothing ->
            50


defaultPriorityWeightsForNonFiction : PriorityWeights
defaultPriorityWeightsForNonFiction =
    { rating = 0.4
    , authors = 0
    , secret = 0.2
    , passion = 0
    , length = 0.4
    }


defaultPriorityWeightsForFiction : PriorityWeights
defaultPriorityWeightsForFiction =
    { rating = 0.6
    , authors = 0
    , secret = 0.1
    , passion = 0
    , length = 0.3
    }


priorityWeightsToList : PriorityWeights -> List Float
priorityWeightsToList weights =
    [ weights.rating, weights.authors, weights.secret, weights.passion, weights.length ]


calculateExpectedMinutesPerPageMultiplier : Dict String Book -> Dict String ReadStatus -> Date -> Maybe Float
calculateExpectedMinutesPerPageMultiplier books readStatues today =
    let
        lastSixMonths =
            readStatues
                |> Dict.filter
                    (\bookId readStatus ->
                        let
                            interval =
                                Date.toTime today - Date.toTime readStatus.startedReading

                            sixMonthsInMilliseconds =
                                6 * 30 * 24 * Time.hour
                        in
                        interval <= sixMonthsInMilliseconds
                    )

        readStatuesAndBooks =
            Utils.leftJoin lastSixMonths books

        averageMinutesPerPagePerBook =
            Dict.values readStatuesAndBooks
                |> List.filterMap
                    (\( readStatus, book ) ->
                        case ( book.numberOfPages, readStatus.finishedReading ) of
                            ( Just numberOfPages, Just finishedReading ) ->
                                Just (calculateAverageMinutesPerPage numberOfPages readStatus.startedReading finishedReading)

                            _ ->
                                Nothing
                    )
    in
    case averageMinutesPerPagePerBook of
        [] ->
            Nothing

        _ ->
            Just (Statistics.median averageMinutesPerPagePerBook)


calculateAverageMinutesPerPage : Int -> Date -> Date -> Float
calculateAverageMinutesPerPage numberOfPages startedReading finishedReading =
    let
        durationInMs =
            Date.toTime finishedReading - Date.toTime startedReading

        durationInMinutes =
            durationInMs / 60000
    in
    durationInMinutes / toFloat numberOfPages


calculateExpectedReadingTimeInMinutes : Float -> Int -> Float
calculateExpectedReadingTimeInMinutes expectedMinutesPerPageMultiplier numberOfPages =
    expectedMinutesPerPageMultiplier * toFloat numberOfPages
