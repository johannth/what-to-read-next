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
    Decode.map8 Book
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "authors" (Decode.list decodeAuthor))
        (Decode.maybe (Decode.field "numberOfPages" Decode.int))
        (Decode.field "averageRating" Decode.float)
        (Decode.maybe (Decode.field "published" Decode.int))


decodeAuthor : Decode.Decoder Author
decodeAuthor =
    Decode.map5 Author
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "averageRating" Decode.float)
        (Decode.field "ratingsCount" Decode.int)
        (Decode.field "textReviewsCount" Decode.int)
