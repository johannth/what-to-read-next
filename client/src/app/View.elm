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


config : Maybe Float -> Table.Config Book Msg
config expectedMinutesPerPageMultiplier =
    Table.config
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ titleColumn
            , Table.stringColumn "Type" (.tags >> normalizedTags >> Set.toList >> List.sort >> String.join ", ")
            , Table.stringColumn "Authors" (\book -> String.join ", " (List.map .name book.authors))
            , Table.intColumn "Average Rating of Authors" (.authors >> State.calculateAuthorsAverageRating)
            , Table.stringColumn "Publication Year" (\book -> Maybe.withDefault "?" (Maybe.map toString book.published))
            , Table.intColumn "Average Rating" (.averageRating >> round)
            , Table.intColumn "# of Ratings" .ratingsCount
            , Table.intColumn "# of Text Reviews" .textReviewsCount
            , Table.intColumn "Popularity" (.ratingsCount >> State.calculatePopularity)
            , Table.intColumn "Passion" State.calculatePassion
            , Table.stringColumn "Number of Pages" (\book -> Maybe.withDefault "?" (Maybe.map toString book.numberOfPages))
            , readingTimeColumn expectedMinutesPerPageMultiplier
            , priorityColumn
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


priorityColumn : Table.Column Book Msg
priorityColumn =
    Table.veryCustomColumn
        { name = "Priority"
        , viewData = \book -> cellWithToolTip (State.renderPriorityFormula book) (State.calculatePriority book |> round |> toString)
        , sorter = Table.decreasingOrIncreasingBy State.calculatePriority
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
