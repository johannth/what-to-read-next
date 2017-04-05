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
                        (round (State.calculateBookLengthRating (Just 100)))
            , test "should return 75 for 300 pages" <|
                \() ->
                    Expect.equal
                        75
                        (State.calculateBookLengthRating (Just 300))
            , test "should return average for 600 pages" <|
                \() ->
                    Expect.equal
                        43
                        (round (State.calculateBookLengthRating (Just 600)))
            , test "should return low for 1000 pages" <|
                \() ->
                    Expect.equal
                        21
                        (round (State.calculateBookLengthRating (Just 1000)))
            ]
        ]
