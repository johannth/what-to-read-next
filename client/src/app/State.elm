module State exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import Api
import Types exposing (..)
import Navigation
import Http
import UrlParser exposing ((<?>))
import Statistics
import Utils
import Set
import Task
import Time


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
                            ("Bad url" ++ url)

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
                    ! (List.map (Api.getGoodreadsBookDetails model.apiHost) batchesOfBookIds)

        LoadGoodreadsBookDetails (Err error) ->
            model ! []

        LoadGoodreadsBookDetails (Ok books) ->
            { model
                | books = Dict.union books model.books
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


calculatePriority : Book -> Float
calculatePriority =
    calculatePriorityWithWeights defaultPriorityWeights


calculatePriorityWithWeights : PriorityWeights -> Book -> Float
calculatePriorityWithWeights weights book =
    Utils.interpolation (priorityWeightsToList weights) (calculatePriorityValues book)


calculatePriorityValues : Book -> List Float
calculatePriorityValues book =
    let
        normalizedBookLength =
            toFloat (calculateBookLengthRating book.numberOfPages)

        popularity =
            toFloat (calculatePopularity book)

        normalizedAverageRating =
            book.averageRating * (popularity / 100)

        authorsAverageRating =
            calculateAuthorsAverageRating book.authors
    in
        [ normalizedAverageRating
        , toFloat authorsAverageRating
        , 100 - popularity
        , toFloat (calculatePassion book)
        , normalizedBookLength
        ]


calculateBookLengthRating : Maybe Int -> Int
calculateBookLengthRating maybeNumberOfPages =
    let
        optimalBookLengthInPages : Float
        optimalBookLengthInPages =
            300

        numberOfPages =
            Maybe.withDefault 400 (Maybe.map toFloat maybeNumberOfPages)
    in
        round (normalizeBookLengthWithParameters optimalBookLengthInPages 0.75 numberOfPages)


normalizeBookLengthWithParameters : Float -> Float -> Float -> Float
normalizeBookLengthWithParameters optimalBookLengthInPages optimalBookLengthScore bookLengthInPages =
    let
        k =
            (optimalBookLengthInPages ^ 2 * optimalBookLengthScore) / (1 - optimalBookLengthScore)
    in
        k / (bookLengthInPages ^ 2 + k) * 100


renderPriorityFormula : Book -> String
renderPriorityFormula book =
    let
        weightsAsStrings =
            List.map toString (priorityWeightsToList defaultPriorityWeights)

        valuesAsString =
            List.map (round >> toString) (calculatePriorityValues book)

        priority =
            round (calculatePriority book)

        formula =
            List.map2 (\x y -> x ++ " * " ++ y) weightsAsStrings valuesAsString |> String.join " + "
    in
        formula ++ " = " ++ (toString priority)


calculateAuthorsAverageRating : List Author -> Int
calculateAuthorsAverageRating authors =
    let
        authorsCount =
            List.length authors

        averageRating =
            Utils.interpolation (List.repeat authorsCount (1 / (toFloat authorsCount))) (List.map .averageRating authors)
    in
        round averageRating


calculatePopularity : Book -> Int
calculatePopularity book =
    let
        averageRatingsCount =
            5000

        ratingForAverage =
            0.6

        f =
            Utils.increasingFunction averageRatingsCount ratingForAverage
    in
        round (f (toFloat book.ratingsCount) * 100)



-- By passion we're trying to capture effort put into book (now by looking at percentage of text reviews)


calculatePassion : Book -> Int
calculatePassion book =
    round (((toFloat book.textReviewsCount) / (toFloat book.ratingsCount)) * 100)


defaultPriorityWeights : PriorityWeights
defaultPriorityWeights =
    { rating = 0.35
    , authors = 0.2
    , secret = 0.2
    , passion = 0.05
    , length = 0.2
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
                                (Date.toTime today) - (Date.toTime readStatus.startedReading)

                            sixMonthsInMilliseconds =
                                6 * 30 * 24 * Time.hour
                        in
                            interval <= sixMonthsInMilliseconds
                    )

        readStatuesAndBooks =
            Utils.leftJoin lastSixMonths books

        averageMinutesPerPagePerBook =
            Dict.values readStatuesAndBooks
                |> (List.filterMap
                        (\( readStatus, book ) ->
                            case ( book.numberOfPages, readStatus.finishedReading ) of
                                ( Just numberOfPages, Just finishedReading ) ->
                                    Just (calculateAverageMinutesPerPage numberOfPages readStatus.startedReading finishedReading)

                                _ ->
                                    Nothing
                        )
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
