module Utils exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode


lift2 : (a -> b) -> (a -> c) -> (a -> ( b, c ))
lift2 f g =
    \x -> ( f x, g x )


leftJoin : Dict String a -> Dict String b -> Dict String ( a, b )
leftJoin dictA dictB =
    Dict.toList dictA
        |> List.filterMap
            (\( key, valueOfTypeA ) ->
                case Dict.get key dictB of
                    Just valueOfTypeB ->
                        Just ( key, ( valueOfTypeA, valueOfTypeB ) )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


interpolation : List Float -> List Float -> Float
interpolation weights values =
    List.map2 (*) weights values
        |> List.sum


batches : Int -> List a -> List (List a)
batches batchSize list =
    let
        batcher : a -> List (List a) -> List (List a)
        batcher item acc =
            let
                accHead =
                    Maybe.withDefault [] (List.head acc)

                accTail =
                    Maybe.withDefault [] (List.tail acc)
            in
            if List.length accHead < batchSize then
                (item :: accHead) :: accTail
            else
                [ item ] :: acc
    in
    List.foldl batcher [ [] ] list


increasingFunction : Float -> Float -> Float -> Float
increasingFunction averageX averageY x =
    let
        a =
            1 / averageX * (1 / (1 - averageY) - 1)
    in
    1 - 1 / (a * x + 1)


maybeHasValue : Maybe a -> Bool
maybeHasValue maybeValue =
    case maybeValue of
        Just _ ->
            True

        Nothing ->
            False


decodeToTuple : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
decodeToTuple decoderA decoderB =
    Decode.map2 (\x y -> ( x, y ))
        decoderA
        decoderB


map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder c -> Decode.Decoder d -> Decode.Decoder e -> Decode.Decoder f -> Decode.Decoder g -> Decode.Decoder h -> Decode.Decoder i -> Decode.Decoder value
map9 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    Decode.map8 (\x1 x2 x3 x4 x5 x6 x7 ( x8, x9 ) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9) decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 (decodeToTuple decoder8 decoder9)


map10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> k -> value) -> Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder c -> Decode.Decoder d -> Decode.Decoder e -> Decode.Decoder f -> Decode.Decoder g -> Decode.Decoder h -> Decode.Decoder i -> Decode.Decoder k -> Decode.Decoder value
map10 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 decoder10 =
    map9 (\x1 x2 x3 x4 x5 x6 x7 x8 ( x9, x10 ) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 (decodeToTuple decoder9 decoder10)


map11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> k -> l -> value) -> Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder c -> Decode.Decoder d -> Decode.Decoder e -> Decode.Decoder f -> Decode.Decoder g -> Decode.Decoder h -> Decode.Decoder i -> Decode.Decoder k -> Decode.Decoder l -> Decode.Decoder value
map11 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 decoder10 decoder11 =
    map10 (\x1 x2 x3 x4 x5 x6 x7 x8 x9 ( x10, x11 ) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 (decodeToTuple decoder10 decoder11)


map12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> k -> l -> m -> value) -> Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder c -> Decode.Decoder d -> Decode.Decoder e -> Decode.Decoder f -> Decode.Decoder g -> Decode.Decoder h -> Decode.Decoder i -> Decode.Decoder k -> Decode.Decoder l -> Decode.Decoder m -> Decode.Decoder value
map12 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 decoder10 decoder11 decoder12 =
    map11 (\x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 ( x11, x12 ) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 decoder10 (decodeToTuple decoder11 decoder12)
