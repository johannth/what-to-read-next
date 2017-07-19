module View exposing (rootView)

import Beta
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
rootView { goodReadsUserIdInputCurrentValue, goodReadsUserId, shelves, books, read, errorMessage, selectedTags, tableState, today, buildInfo } =
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


config : Maybe Float -> Table.Config Book Msg
config expectedMinutesPerPageMultiplier =
    if True then
        Table.config
            { toId = .id
            , toMsg = SetTableState
            , columns =
                [ titleColumn
                , Table.stringColumn "Authors" (\book -> String.join ", " (List.map .name book.authors))
                , Table.stringColumn "Type" (.tags >> normalizedTags >> Set.toList >> List.sort >> String.join ", ")
                , prettyFloatColumn "Data Confidence" (estimateBetaDistributionParametersForBook >> (Beta.percentiles 5 >> (\( fifth, ninetyFifth ) -> 1 - (ninetyFifth - fifth))))
                , prettyFloatColumn "Rating (0-1)" meanRatingForBook
                , prettyFloatColumn "Rating (Worst)" (estimateBetaDistributionParametersForBook >> (Beta.percentiles 5 >> Tuple.first))
                , prettyFloatColumn "Rating (Best)" (estimateBetaDistributionParametersForBook >> Beta.percentiles 5 >> Tuple.second)
                , prettyFloatColumn "Variance" (varianceOfRatingsForBook >> Maybe.withDefault -1)
                , prettyFloatColumn "Agreement" (estimateBetaDistributionParametersForBook >> (\{ alpha, beta } -> abs (alpha - beta)))
                , prettyFloatColumn "Secret (0-1)" State.calculateSecretRating
                , prettyFloatColumn "Shortness (0-1)" (.numberOfPages >> State.calculateBookLengthRating)
                , readingTimeColumn expectedMinutesPerPageMultiplier
                , prettyFloatColumn "Priority" State.calculatePriority
                ]
            }
    else
        Table.config
            { toId = .id
            , toMsg = SetTableState
            , columns =
                [ titleColumn
                , Table.stringColumn "Type" (.tags >> normalizedTags >> Set.toList >> List.sort >> String.join ", ")
                , Table.stringColumn "Authors" (\book -> String.join ", " (List.map .name book.authors))
                , Table.stringColumn "Publication Year" (\book -> Maybe.withDefault "?" (Maybe.map toString book.published))
                , Table.intColumn "Average Rating" (meanRatingForBook >> round)
                , Table.intColumn "# of Ratings" ratingsCountForBook
                , Table.intColumn "# of Text Reviews" .textReviewsCount
                , Table.stringColumn "Number of Pages" (\book -> Maybe.withDefault "?" (Maybe.map toString book.numberOfPages))
                , readingTimeColumn expectedMinutesPerPageMultiplier
                , prettyFloatColumn "Priority" State.calculatePriority
                ]
            }


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ String.slice 0 8 buildInfo.version ++ "-" ++ buildInfo.tier)


titleColumn : Table.Column Book Msg
titleColumn =
    Table.veryCustomColumn
        { name = "Title"
        , viewData = \book -> linkCell book.title book.url
        , sorter = Table.increasingOrDecreasingBy .title
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


readingTimeColumn : Maybe Float -> Table.Column Book Msg
readingTimeColumn expectedMinutesPerPageMultiplier =
    let
        readingTime =
            \book ->
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
