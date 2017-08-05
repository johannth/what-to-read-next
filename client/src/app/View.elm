module View exposing (rootView)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import Round
import Set exposing (Set)
import State
import Table
import Types exposing (..)


isSubset : Set comparable -> Set comparable -> Bool
isSubset setA setB =
    Set.diff setA setB |> Set.isEmpty


bookHasTag : Set String -> Book -> Bool
bookHasTag selectedTags book =
    if Set.isEmpty selectedTags == True then
        True
    else
        isSubset selectedTags (normalizedTags book.tags)


rootView : Model -> Html Msg
rootView { goodReadsUserIdInputCurrentValue, goodReadsUserId, shelves, books, ratings, read, errorMessage, selectedTags, tableState, today, buildInfo } =
    let
        expectedMinutesPerPageMultiplier =
            today |> Maybe.andThen (State.calculateExpectedMinutesPerPageMultiplier books read)

        list =
            Dict.get "to-read" shelves
                |> Maybe.withDefault []

        expandedList =
            List.filterMap (\bookId -> Dict.get bookId books) list

        tags =
            Set.fromList [ "fiction", "non-fiction" ]

        filteredList =
            List.filter (bookHasTag selectedTags) expandedList
                |> List.map
                    (\book ->
                        ( book
                        , case Dict.get book.id ratings of
                            Just rating ->
                                rating

                            Nothing ->
                                calculateRatingsForBook book
                        )
                    )
    in
    div [ id "content" ]
        [ h1 [ id "title" ] [ text "What should I read next?" ]
        , div [ id "body" ]
            [ userIdTextInput goodReadsUserIdInputCurrentValue
            , userIdView goodReadsUserId
            , listCountView expandedList
            , readingSpeedView expectedMinutesPerPageMultiplier
            , div [ id "tags" ] (Set.toList tags |> List.sort |> List.map (tagView selectedTags))
            , div [ id "list" ]
                [ case list of
                    [] ->
                        text
                            (case goodReadsUserId of
                                Just _ ->
                                    "Loading..."

                                _ ->
                                    ""
                            )

                    list ->
                        Table.view (config expectedMinutesPerPageMultiplier) tableState filteredList
                ]
            ]
        , case errorMessage of
            Just message ->
                div [ id "error" ] [ text message ]

            _ ->
                div [ id "error" ] []
        , div [ id "footer" ]
            [ buildInfoView buildInfo
            ]
        ]


config : Maybe Float -> Table.Config ( Book, CachedRating ) Msg
config expectedMinutesPerPageMultiplier =
    Table.config
        { toId = Tuple.first >> .id
        , toMsg = SetTableState
        , columns =
            [ titleColumn
            , Table.stringColumn "Authors" (\( book, rating ) -> String.join ", " (List.map .name book.authors))
            , Table.stringColumn "Type" (Tuple.first >> .tags >> normalizedTags >> Set.toList >> List.sort >> String.join ", ")
            , Table.stringColumn "Year" (Tuple.first >> .published >> Maybe.map toString >> Maybe.withDefault "")
            , Table.stringColumn "# of Pages" (Tuple.first >> .numberOfPages >> Maybe.map toString >> Maybe.withDefault "")
            , prettyFloatColumn "Rating (Worst)" (Tuple.second >> .worstCaseRating)
            , prettyFloatColumn "Rating (Best)" (Tuple.second >> .bestCaseRating)
            , prettyFloatColumn "Rating (0-1)" (Tuple.second >> .meanRating)
            , prettyFloatColumn "Secret (0-1)" (Tuple.first >> State.calculateSecretRating)
            , prettyFloatColumn "Shortness (0-1)" (Tuple.first >> .numberOfPages >> State.calculateBookLengthRating)
            , readingTimeColumn expectedMinutesPerPageMultiplier
            , prettyFloatColumn "Priority (Worst)" (State.calculatePriority State.WorstCase)
            , prettyFloatColumn "Priority (Best)" (State.calculatePriority State.BestCase)
            , prettyFloatColumn "Priority (0-1)" (State.calculatePriority State.AverageCase)
            ]
        }


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ String.slice 0 8 buildInfo.version ++ "-" ++ buildInfo.tier)


titleColumn : Table.Column ( Book, CachedRating ) Msg
titleColumn =
    Table.veryCustomColumn
        { name = "Title"
        , viewData = \( book, _ ) -> linkCell book.title book.url
        , sorter = Table.increasingOrDecreasingBy (Tuple.first >> .title)
        }


linkCell : String -> String -> Table.HtmlDetails Msg
linkCell title url =
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text title ]
        ]


prettyPrintReadingTime : Float -> String
prettyPrintReadingTime readingTimeInMinutes =
    if readingTimeInMinutes < 60 then
        Round.round 1 readingTimeInMinutes ++ " min"
    else if readingTimeInMinutes < (24 * 60) then
        Round.round 1 (readingTimeInMinutes / 60) ++ " hours"
    else
        Round.round 1 (readingTimeInMinutes / (24 * 60)) ++ " days"


readingTimeColumn : Maybe Float -> Table.Column ( Book, CachedRating ) Msg
readingTimeColumn expectedMinutesPerPageMultiplier =
    let
        readingTime =
            \( book, rating ) ->
                case ( book.numberOfPages, expectedMinutesPerPageMultiplier ) of
                    ( Just numberOfPages, Just expectedMinutesPerPageMultiplier ) ->
                        State.calculateExpectedReadingTimeInMinutes expectedMinutesPerPageMultiplier numberOfPages |> prettyPrintReadingTime

                    _ ->
                        "?"
    in
    Table.stringColumn "Expected Reading Time" readingTime


prettyFloatColumn : String -> (data -> Float) -> Table.Column data msg
prettyFloatColumn name value =
    Table.customColumn
        { name = name
        , viewData = value >> Round.round 3
        , sorter = Table.decreasingOrIncreasingBy value
        }


cellWithToolTip : String -> String -> Table.HtmlDetails Msg
cellWithToolTip tooltip value =
    Table.HtmlDetails []
        [ span [ title tooltip ] [ text value ]
        ]


userIdView : Maybe String -> Html Msg
userIdView maybeUserId =
    div [ id "users" ]
        [ case maybeUserId of
            Just userId ->
                span [ class "user-link" ]
                    [ a [ target "_blank", href ("https://www.goodreads.com/review/list/" ++ userId ++ "?shelf=to-read") ]
                        [ text userId
                        ]
                    , a [ class "user-remove-button", href "#", Html.Events.onClick (ClearList userId) ] [ text "x" ]
                    ]

            _ ->
                span [] []
        ]


userIdTextInput : String -> Html Msg
userIdTextInput currentValue =
    let
        properties =
            [ placeholder "Enter GoodReads userId", onEnter LookupWatchList, Html.Events.onInput UserIdInput, value currentValue ]
    in
    div [ id "user-id-input" ]
        [ input properties []
        ]


onEnter : (String -> Msg) -> Attribute Msg
onEnter msg =
    let
        isEnter : Int -> Decode.Decoder String
        isEnter code =
            if code == 13 then
                Decode.succeed "ENTER pressed"
            else
                Decode.fail "not ENTER"

        decodeEnter =
            Decode.andThen isEnter Html.Events.keyCode

        decodeEnterWithValue : Decode.Decoder Msg
        decodeEnterWithValue =
            Decode.map2 (\key value -> msg value)
                decodeEnter
                Html.Events.targetValue
    in
    Html.Events.on "keydown" decodeEnterWithValue


readingSpeedView : Maybe Float -> Html Msg
readingSpeedView maybeAverageMinutesPerPage =
    div [ id "readingSpeed" ]
        [ case maybeAverageMinutesPerPage of
            Just averageMinutesPerPage ->
                text ("It takes you " ++ prettyPrintReadingTime (averageMinutesPerPage * 100) ++ " to read 100 pages based on your reading history")

            _ ->
                text ""
        ]


listCountView : List Book -> Html Msg
listCountView list =
    div [ id "listCount" ]
        (case List.length list of
            1 ->
                [ text "There is only one book on the reading list." ]

            numberOfBooks ->
                [ text ("There are " ++ toString numberOfBooks ++ " books on the reading list.") ]
        )


tagView : Set String -> String -> Html Msg
tagView selectedTags tag =
    let
        isSelected =
            if Set.isEmpty selectedTags then
                True
            else
                Set.member tag selectedTags
    in
    a [ classList [ ( "tag", True ), ( "selected", isSelected ) ], href "#", Html.Events.onClick (ToggleTagFilter tag) ] [ text tag ]
