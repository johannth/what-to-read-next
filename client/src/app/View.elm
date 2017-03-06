module View exposing (rootView)

import Table
import Set exposing (Set)
import Dict
import State
import Json.Decode as Decode
import Html.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Dict exposing (Dict)


rootView : Model -> Html Msg
rootView { goodReadsUserIdInputCurrentValue, lists, books, tableState, buildInfo } =
    let
        list =
            Dict.values lists
                |> List.map Set.fromList
                |> List.foldl Set.union Set.empty
                |> Set.toList

        expandedList =
            List.filterMap (\bookId -> Dict.get bookId books) list
    in
        div [ id "content" ]
            [ h1 [ id "title" ] [ text "What should I read next?" ]
            , div [ id "body" ]
                [ userIdTextInput goodReadsUserIdInputCurrentValue
                , div [ id "users" ] (Dict.keys lists |> List.map userIdView)
                , div [ id "list" ]
                    [ case list of
                        [] ->
                            text
                                (if Dict.size lists > 0 then
                                    "Loading..."
                                 else
                                    ""
                                )

                        list ->
                            Table.view config tableState expandedList
                    ]
                ]
            , div [ id "footer" ]
                [ buildInfoView buildInfo
                ]
            ]


config : Table.Config Book Msg
config =
    Table.config
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ titleColumn
            , Table.stringColumn "Authors" (\book -> String.join ", " book.authors)
            , Table.stringColumn "Publication Year" (\book -> Maybe.withDefault "?" (Maybe.map toString book.published))
            , Table.intColumn "Average Rating" (.averageRating >> round)
            , Table.stringColumn "Number of Pages" (\book -> Maybe.withDefault "?" (Maybe.map toString book.numberOfPages))
            , priorityColumn
            ]
        }


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ (String.slice 0 8 buildInfo.version) ++ "-" ++ buildInfo.tier)


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


priorityColumn : Table.Column Book Msg
priorityColumn =
    Table.customColumn
        { name = "Priority"
        , viewData = State.calculatePriority >> round >> toString
        , sorter = Table.decreasingOrIncreasingBy State.calculatePriority
        }


userIdView : String -> Html Msg
userIdView userId =
    span [ class "user-link" ]
        [ a [ target "_blank", href ("https://www.goodreads.com/review/list/" ++ userId ++ "?shelf=to-read") ]
            [ text userId
            ]
        , a [ class "user-remove-button", href "#", Html.Events.onClick (ClearList userId) ] [ text "x" ]
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
