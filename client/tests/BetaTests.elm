module BetaTests exposing (..)

import Beta
import Expect
import Test exposing (..)
import TestUtils exposing (..)


all : Test
all =
    describe "Beta Test Suite"
        [ describe "lnOfGamma"
            [ test "lnOfGamma(1) = 0" <|
                \() ->
                    equalFloat
                        0.0
                        (Beta.lnOfGamma 1)
            , test "lnOfGamma(2) = 0" <|
                \() ->
                    equalFloat
                        0.0
                        (Beta.lnOfGamma 2)
            , test "lnOfGamma(3) = log (3)" <|
                \() ->
                    equalFloat
                        (logBase e 2)
                        (Beta.lnOfGamma 3)
            , test "lnOfGamma(4) = log (6)" <|
                \() ->
                    equalFloat
                        (logBase e 6)
                        (Beta.lnOfGamma 4)
            , test "lnOfGamma(1.5) = -0.120782" <|
                \() ->
                    equalFloat
                        -0.120782
                        (Beta.lnOfGamma 1.5)
            ]
        , describe "incompleteBeta"
            [ test "incompleteBeta(0, 1, 1) = 0" <|
                \() ->
                    equalFloat
                        0.0
                        (Beta.incompleteBeta { alpha = 1, beta = 1 } 0)
            , test "incompleteBeta(1, 1, 1) = 1" <|
                \() ->
                    equalFloat
                        1
                        (Beta.incompleteBeta { alpha = 1, beta = 1 } 1)
            , test "incompleteBeta(0.5, 2, 1) = 0.125" <|
                \() ->
                    equalFloat
                        0.25
                        (Beta.incompleteBeta { alpha = 2, beta = 1 } 0.5)
            , test "incompleteBeta(0, 1, 3) = 0" <|
                \() ->
                    equalFloat
                        0
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 0)
            , test "incompleteBeta(0.5, 1, 3) = 0.875" <|
                \() ->
                    equalFloat
                        0.875
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 0.5)
            , test "incompleteBeta(1, 1, 3) = 1" <|
                \() ->
                    equalFloat
                        1
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 1)
            ]
        ]
