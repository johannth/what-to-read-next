module Api exposing (getGoodreadsToReadData)

import Json.Decode as Decode
import Http
import Types exposing (..)
import Utils exposing (map9)


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


getGoodreadsToReadData : String -> String -> Cmd Msg
getGoodreadsToReadData apiHost userId =
    Http.send (LoadGoodreadsToReadList userId) <|
        Http.get (apiUrl apiHost ("/api/goodreads?userId=" ++ userId)) decodeReadingList


decodeReadingList : Decode.Decoder (List Book)
decodeReadingList =
    Decode.at [ "data", "list" ] (Decode.list decodeBook)


decodeBook : Decode.Decoder Book
decodeBook =
    map9 Book
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "isbn" Decode.string)
        (Decode.field "authors" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "numberOfPages" Decode.int))
        (Decode.field "averageRating" Decode.float)
        (Decode.maybe (Decode.field "published" Decode.int))
