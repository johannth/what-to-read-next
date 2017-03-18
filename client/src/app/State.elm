module State exposing (init, update, calculatePriority, calculatePriorityWithWeights, defaultPriorityWeights, calculateAuthorsAverageRating, calculatePopularity, renderPriorityFormula, calculatePassion)

import Dict exposing (Dict)
import Api
import Types exposing (..)
import Utils
import Navigation
import Http
import UrlParser exposing ((<?>))


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        userIdsFromPath =
            parseGoodReadsUserIdFromPath location

        initialModel =
            emptyModel flags

        initalShelves =
            Dict.fromList (List.map (Utils.lift2 identity (always Dict.empty)) userIdsFromPath)
    in
        { initialModel | shelves = initalShelves } ! List.concatMap (Api.fetchUserData initialModel.apiHost) userIdsFromPath


parseGoodReadsUserIdFromPath : Navigation.Location -> List String
parseGoodReadsUserIdFromPath location =
    let
        pathParser =
            UrlParser.oneOf [ UrlParser.s "", UrlParser.s "what-to-read-next" ]
    in
        UrlParser.parsePath (pathParser <?> UrlParser.stringParam "goodReadsUserId") location
            |> Maybe.andThen identity
            |> Maybe.map (String.split ",")
            |> Maybe.withDefault []


updatedUrl : Model -> String
updatedUrl model =
    "?goodReadsUserId=" ++ (String.join "," (Dict.keys model.shelves))


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
                        , shelves = Dict.insert goodReadsUserId Dict.empty model.shelves
                    }
            in
                newModel
                    ! (Api.fetchUserData model.apiHost goodReadsUserId
                        ++ [ Navigation.modifyUrl (updatedUrl newModel) ]
                      )

        ClearList goodReadsUserId ->
            let
                newModel =
                    { model | shelves = Dict.remove goodReadsUserId model.shelves }
            in
                newModel ! [ Navigation.modifyUrl (updatedUrl newModel) ]

        LoadGoodreadsToReadList goodReadsUserId shelf (Err error) ->
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

        LoadGoodreadsToReadList goodReadsUserId shelf (Ok ( list, books, readStatuses )) ->
            let
                currentShelves =
                    Maybe.withDefault Dict.empty (Dict.get goodReadsUserId model.shelves)

                newShelves =
                    Dict.insert shelf list currentShelves
            in
                { model
                    | shelves = Dict.insert goodReadsUserId newShelves model.shelves
                    , books = Dict.union books model.books
                    , read = Dict.insert goodReadsUserId readStatuses model.read
                }
                    ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        UrlChange newLocation ->
            model ! []


calculatePriority : Book -> Float
calculatePriority =
    calculatePriorityWithWeights defaultPriorityWeights


calculatePriorityWithWeights : PriorityWeights -> Book -> Float
calculatePriorityWithWeights weights book =
    interpolation (priorityWeightsToList weights) (calculatePriorityValues book)


calculatePriorityValues : Book -> List Float
calculatePriorityValues book =
    let
        normalizedBookLength =
            normalizeBookLength (Maybe.withDefault optimalBookLengthInPages (Maybe.map toFloat book.numberOfPages))

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


optimalBookLengthInPages : Float
optimalBookLengthInPages =
    300


interpolation : List Float -> List Float -> Float
interpolation weights values =
    List.map2 (*) weights values
        |> List.sum


calculateAuthorsAverageRating : List Author -> Int
calculateAuthorsAverageRating authors =
    let
        authorsCount =
            List.length authors

        averageRating =
            interpolation (List.repeat authorsCount (1 / (toFloat authorsCount))) (List.map .averageRating authors)
    in
        round averageRating


increasingFunction : Float -> Float -> Float -> Float
increasingFunction averageX averageY x =
    let
        a =
            1 / averageX * (1 / (1 - averageY) - 1)
    in
        1 - 1 / (a * x + 1)


calculatePopularity : Book -> Int
calculatePopularity book =
    let
        averageRatingsCount =
            5000

        ratingForAverage =
            0.6

        f =
            increasingFunction averageRatingsCount ratingForAverage
    in
        round (f (toFloat book.ratingsCount) * 100)



-- By passion we're trying to capture effort put into book (now by looking at percentage of text reviews)


calculatePassion : Book -> Int
calculatePassion book =
    round (((toFloat book.textReviewsCount) / (toFloat book.ratingsCount)) * 100)


normalizeBookLength : Float -> Float
normalizeBookLength =
    normalizeBookLengthWithParameters optimalBookLengthInPages 0.5


normalizeBookLengthWithParameters : Float -> Float -> Float -> Float
normalizeBookLengthWithParameters optimalBookLengthInPages optimalBookLengthScore bookLengthInPages =
    let
        k =
            (optimalBookLengthInPages ^ 2 * optimalBookLengthScore) / (1 - optimalBookLengthScore)
    in
        k / (bookLengthInPages ^ 2 + k) * 100


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
