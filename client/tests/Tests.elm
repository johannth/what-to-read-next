module Tests exposing (..)

import Expect
import Fuzz exposing (int, list, string, tuple)
import State exposing (..)
import String
import Test exposing (..)
import Types exposing (..)


custom : (a -> b -> Result String ()) -> a -> b -> Expect.Expectation
custom f expected actual =
    case f expected actual of
        Ok () ->
            Expect.pass

        Err str ->
            [ toString actual
            , "╷"
            , "│ " ++ str
            , "╵"
            , toString expected
            ]
                |> String.join "\n"
                |> Expect.fail


equalFloatWithEpsilon : Float -> Float -> Float -> Expect.Expectation
equalFloatWithEpsilon epsilon x y =
    custom
        (\a b ->
            if abs (x - y) < epsilon then
                Ok ()
            else
                Err ("Tolerance: " ++ toString epsilon)
        )
        x
        y


equalFloat : Float -> Float -> Expect.Expectation
equalFloat x y =
    equalFloatWithEpsilon 0.0001 x y


all : Test
all =
    describe "Calculate Priority Test Suite"
        [ describe "defaultPriorityWeights"
            [ test "should sum to 1" <|
                \() ->
                    equalFloat
                        1.0
                        (List.sum (State.priorityWeightsToList State.defaultPriorityWeightsForFiction))
            ]
        , describe "calculateBookLengthRating"
            [ test "should return 1 for 0 pages" <|
                \() ->
                    Expect.equal
                        1
                        (State.calculateBookLengthRating (Just 0))
            , test "should return good rating for 100 pages" <|
                \() ->
                    equalFloat
                        0.9642857142857143
                        (State.calculateBookLengthRating (Just 100))
            , test "should return 0.75 for 300 pages" <|
                \() ->
                    Expect.equal
                        0.75
                        (State.calculateBookLengthRating (Just 300))
            , test "should return average for 600 pages" <|
                \() ->
                    equalFloat
                        0.42857
                        (State.calculateBookLengthRating (Just 600))
            , test "should return low for 1000 pages" <|
                \() ->
                    equalFloat
                        0.2125
                        (State.calculateBookLengthRating (Just 1000))
            , test "should return something for missing pages" <|
                \() ->
                    equalFloat
                        0.6279
                        (State.calculateBookLengthRating Nothing)
            ]
        , describe "calculatePopularity"
            [ test "should return ~1 for very popular books" <|
                \() ->
                    equalFloat
                        0.995
                        (State.calculatePopularity 3000000)
            , test "should return high rating for fairly popular books" <|
                \() ->
                    equalFloat
                        0.909
                        (State.calculatePopularity 150000)
            , test "should return medium rating for average books" <|
                \() ->
                    equalFloat
                        0.5
                        (State.calculatePopularity 15000)
            , test "should return low for seldomly rated books" <|
                \() ->
                    equalFloat
                        0.25
                        (State.calculatePopularity 5000)
            , test "should return 0 for never rated books" <|
                \() ->
                    equalFloat
                        0
                        (State.calculatePopularity 0)
            ]
        ]
