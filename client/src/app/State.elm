module State exposing (init, update, calculatePriority, calculatePriorityWithWeights, defaultPriorityWeights)

import Dict
import Api
import Types exposing (..)
import Utils
import Navigation
import UrlParser exposing ((<?>))


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        userIdsFromPath =
            parseGoodReadsUserIdFromPath location

        initialModel =
            emptyModel flags

        initalLists =
            Dict.fromList (List.map (Utils.lift2 identity (always [])) userIdsFromPath)
    in
        { initialModel | lists = initalLists } ! List.map (Api.getGoodreadsToReadData initialModel.apiHost) userIdsFromPath


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
    "?goodReadsUserId=" ++ (String.join "," (Dict.keys model.lists))


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
                        , lists = Dict.insert goodReadsUserId [] model.lists
                    }
            in
                newModel
                    ! [ Api.getGoodreadsToReadData model.apiHost goodReadsUserId, Navigation.modifyUrl (updatedUrl newModel) ]

        ClearList goodReadsUserId ->
            let
                newModel =
                    { model | lists = Dict.remove goodReadsUserId model.lists }
            in
                newModel ! [ Navigation.modifyUrl (updatedUrl newModel) ]

        LoadGoodreadsToReadList goodReadsUserId (Err error) ->
            model ! []

        LoadGoodreadsToReadList goodReadsUserId (Ok books) ->
            let
                listOfIds =
                    List.map .id books

                newBooks =
                    (List.map (Utils.lift2 .id identity) books)
                        |> Dict.fromList
            in
                { model
                    | lists = Dict.insert goodReadsUserId listOfIds model.lists
                    , books = Dict.union newBooks model.books
                }
                    ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        UrlChange newLocation ->
            model ! []


calculatePriority : Book -> Float
calculatePriority =
    calculatePriorityWithWeights defaultPriorityWeights


optimalBookLengthInPages =
    300


calculatePriorityWithWeights : PriorityWeights -> Book -> Float
calculatePriorityWithWeights weights book =
    let
        normalizedBookLength =
            normalizeBookLength (Maybe.withDefault optimalBookLengthInPages (Maybe.map toFloat book.numberOfPages))
    in
        weights.length * normalizedBookLength + weights.rating * book.averageRating


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
    { rating = 0.7
    , length = 0.3
    }
