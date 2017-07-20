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
        ]
