module Tests exposing (..)

import Beta
import Expect
import Fuzz exposing (int, list, string, tuple)
import State exposing (..)
import String
import Test exposing (..)
import TestUtils exposing (..)
import Types exposing (..)


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
        , describe "estimateBetaDistributionParameters"
            [ test "should return (mean, mean * (1 - mean) - 1.0e-12) if we don't have the rating distribution" <|
                \() ->
                    Expect.equal
                        (Beta.estimateBetaDistributionParameters 0.6 (0.6 * (1 - 0.6) - 1.0e-12))
                        (estimateBetaDistributionParameters Nothing 0.6)
            , test "should return (0.5, 0.5 * (1 - 0.5) - 1.0e-12) if we have the rating distribution but no ratings" <|
                \() ->
                    Expect.equal
                        (Beta.estimateBetaDistributionParameters 0.5 (0.5 * (1 - 0.5) - 1.0e-12))
                        (estimateBetaDistributionParameters (Just (RatingDistribution 0 0 0 0 0)) 0)
            , test "should return (1 - 1.0e-11, (1 - 1.0e-11) * (1 - (1 - 1.0e-11)) - 1.0e-12) if we have the rating distribution with mean of 1" <|
                \() ->
                    let
                        adjustedMean =
                            0.99
                    in
                    Expect.equal
                        (Beta.estimateBetaDistributionParameters adjustedMean (adjustedMean * (1 - adjustedMean) - 1.0e-12))
                        (estimateBetaDistributionParameters Nothing 1)
            ]
        ]
