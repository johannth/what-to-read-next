module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import State exposing (..)
import Types exposing (..)


all : Test
all =
    describe "Calculate Priority Test Suite"
        [ describe "calculateBookLengthRating"
            [ test "should return 100 for 0 pages" <|
                \() ->
                    Expect.equal
                        100
                        (State.calculateBookLengthRating (Just 0))
            , test "should return good rating for 100 pages" <|
                \() ->
                    Expect.equal
                        96
                        (State.calculateBookLengthRating (Just 100))
            , test "should return 75 for 300 pages" <|
                \() ->
                    Expect.equal
                        75
                        (State.calculateBookLengthRating (Just 300))
            , test "should return average for 600 pages" <|
                \() ->
                    Expect.equal
                        43
                        (State.calculateBookLengthRating (Just 600))
            , test "should return low for 1000 pages" <|
                \() ->
                    Expect.equal
                        21
                        (State.calculateBookLengthRating (Just 1000))
            , test "should return something for missing pages" <|
                \() ->
                    Expect.equal
                        63
                        (State.calculateBookLengthRating Nothing)
            ]
        , describe "calculatePopularity"
            [ test "should return 100 for very popular books" <|
                \() ->
                    Expect.equal
                        100
                        (State.calculatePopularity 3000000)
            , test "should return high rating for fairly popular books" <|
                \() ->
                    Expect.equal
                        91
                        (State.calculatePopularity 150000)
            , test "should return medium rating for average books" <|
                \() ->
                    Expect.equal
                        50
                        (State.calculatePopularity 15000)
            , test "should return low for seldomly rated books" <|
                \() ->
                    Expect.equal
                        25
                        (State.calculatePopularity 5000)
            , test "should return 0 for never rated books" <|
                \() ->
                    Expect.equal
                        0
                        (State.calculatePopularity 0)
            ]
        ]
