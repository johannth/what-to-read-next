module Api exposing (fetchUserData, getGoodreadsBookDetails)

import Json.Decode as Decode
import Dict exposing (Dict)
import Http
import Types exposing (..)
import Utils exposing (map11)
import Date exposing (Date)
import Set


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


fetchUserData : String -> String -> List (Cmd Msg)
fetchUserData apiHost userId =
    [ getGoodreadsShelfData apiHost "to-read" userId
    , getGoodreadsShelfData apiHost "read" userId
    ]


getGoodreadsBookDetails : String -> List String -> Cmd Msg
getGoodreadsBookDetails apiHost bookIds =
    Http.send LoadGoodreadsBookDetails <|
        Http.get (apiUrl apiHost ("/api/goodreads/books?bookIds=" ++ (String.join "," bookIds))) (Decode.at [ "data", "books" ] decodeBooks)


getGoodreadsShelfData : String -> String -> String -> Cmd Msg
getGoodreadsShelfData apiHost shelf userId =
    Http.send (LoadGoodreadsShelf userId shelf) <|
        Http.get (apiUrl apiHost ("/api/goodreads?userId=" ++ userId ++ "&shelf=" ++ shelf)) decodeReadingList


decodeReadingList : Decode.Decoder ( List String, Dict String Book, Dict String ReadStatus )
decodeReadingList =
    Decode.map3 (\x y z -> ( x, y, z ))
        (Decode.at [ "data", "list" ] (Decode.list Decode.string))
        (Decode.at [ "data", "books" ] decodeBooks)
        (Decode.at [ "data", "readStatus" ] decodeReadStatuses)


decodeBooks : Decode.Decoder (Dict String Book)
decodeBooks =
    Decode.dict decodeBook


decodeBook : Decode.Decoder Book
decodeBook =
    map11 Book
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "authors" (Decode.list decodeAuthor))
        (Decode.field "numberOfPages" (Decode.nullable Decode.int))
        (Decode.field "averageRating" Decode.float)
        (Decode.field "ratingsCount" Decode.int)
        (Decode.field "textReviewsCount" Decode.int)
        (Decode.maybe (Decode.field "published" Decode.int))
        (Decode.map (Maybe.withDefault Set.empty)
            (Decode.maybe (Decode.field "tags" (Decode.map Set.fromList (Decode.list Decode.string))))
        )


decodeAuthor : Decode.Decoder Author
decodeAuthor =
    Decode.map5 Author
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "averageRating" Decode.float)
        (Decode.field "ratingsCount" Decode.int)
        (Decode.field "textReviewsCount" Decode.int)


decodeReadStatuses : Decode.Decoder (Dict String ReadStatus)
decodeReadStatuses =
    Decode.dict decodeReadStatus


decodeDate : Decode.Decoder (Maybe Date)
decodeDate =
    let
        converter : Maybe String -> Maybe Date
        converter =
            Maybe.withDefault "" >> Date.fromString >> Result.toMaybe
    in
        Decode.map converter (Decode.nullable Decode.string)


decodeReadStatus : Decode.Decoder ReadStatus
decodeReadStatus =
    Decode.map2 ReadStatus
        (Decode.field "startedReading" (Decode.map (Maybe.withDefault (Date.fromTime 0)) decodeDate))
        (Decode.field "finishedReading" decodeDate)
