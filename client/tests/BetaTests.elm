module BetaTests exposing (..)

import Beta
import Expect
import Test exposing (..)
import TestUtils exposing (..)


eps =
    0.0000000000000001


all : Test
all =
    describe "Beta Test Suite"
        [ describe "lnOfGamma"
            [ test "lnOfGamma(1) = 0" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.0
                        (Beta.lnOfGamma 1)
            , test "lnOfGamma(2) = 0" <|
                \() ->
                    equalFloatWithEpsilon eps
                        -4.440892098500626e-16
                        (Beta.lnOfGamma 2)
            , test "lnOfGamma(3) = log (3)" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.6931471805599443
                        (Beta.lnOfGamma 3)
            , test "lnOfGamma(4) = log (6)" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1.791759469228055
                        (Beta.lnOfGamma 4)
            , test "lnOfGamma(1.5) = -0.120782" <|
                \() ->
                    equalFloatWithEpsilon eps
                        -0.12078223763524987
                        (Beta.lnOfGamma 1.5)
            , test "-lnOfGamma(1) - lnOfGamma(3) + lnOfGamma(1 + 3) = 1.0986122886681107" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1.0986122886681107
                        (-1 * Beta.lnOfGamma 1 - Beta.lnOfGamma 3 + Beta.lnOfGamma (1 + 3))
            ]
        , describe "incompleteBeta"
            [ test "incompleteBeta(0, 1, 1) = 0" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.0
                        (Beta.incompleteBeta { alpha = 1, beta = 1 } 0)
            , test "incompleteBeta(1, 1, 1) = 1" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1
                        (Beta.incompleteBeta { alpha = 1, beta = 1 } 1)
            , test "incompleteBeta(0.5, 2, 1) = 0.24999999999999983" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.24999999999999983
                        (Beta.incompleteBeta { alpha = 2, beta = 1 } 0.5)
            , test "incompleteBeta(0, 1, 3) = 0" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 0)
            , test "incompleteBeta(0.5, 1, 3) = 0.8749999999999998" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.8749999999999998
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 0.5)
            , test "incompleteBeta(1, 1, 3) = 1" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 1)
            , test "incompleteBeta(0.2705288309095449, 1, 3) = 0.6118278296629177" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.6118278296629177
                        (Beta.incompleteBeta { alpha = 1, beta = 3 } 0.2705288309095449)
            ]
        , describe "betaContinuedFraction"
            [ test "betaContinuedFraction(1 - 0.5, 3, 1) = 2" <|
                \() ->
                    equalFloatWithEpsilon eps
                        2
                        (Beta.betaContinuedFraction { alpha = 3, beta = 1 } (1 - 0.5))
            , test "betaContinuedFraction(0.2705288309095449, 1, 3) = 1.9420929924477495" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1.9420929924477495
                        (Beta.betaContinuedFraction { alpha = 1, beta = 3 } 0.2705288309095449)
            ]
        , describe "inverseCDF"
            [ test "inverseCDF {alpha=1 beta=1} 0 = 0" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.0
                        (Beta.inverseCDF { alpha = 1, beta = 1 } 0)
            , test "inverseCDF {alpha=1 beta=1} 1 = 1" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1
                        (Beta.inverseCDF { alpha = 1, beta = 1 } 1)
            , test "inverseCDF {alpha=1 beta=1} 0.5 = 0.5000000000000003" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.5000000000000002
                        (Beta.inverseCDF { alpha = 1, beta = 1 } 0.5)
            , test "inverseCDF {alpha=1 beta=3} 0.5 = 0.2062994740158999" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.2062994740158999
                        (Beta.inverseCDF { alpha = 1, beta = 3 } 0.5)
            , test "inverseCDF {alpha=3 beta=1} 0.5 = 0.79370052598419" <|
                \() ->
                    equalFloatWithEpsilon eps
                        0.7937005259841001
                        (Beta.inverseCDF { alpha = 3, beta = 1 } 0.5)
            ]
        , describe "estimateBetaDistributionParameters"
            [ test "estimateBetaDistributionParameters 0.5 1 = 0" <|
                \() ->
                    Expect.equal
                        { alpha = 0.002008032128514081, beta = 0.002008032128514081 }
                        (Beta.estimateBetaDistributionParameters 0.5 (0.5 * (1 - 0.5) - 0.001))
            ]
        , describe "percentiles"
            [ test "percentiles 0.05 { alpha = -0.375, beta = -0.375} = (0, 1)" <|
                \() ->
                    Expect.equal
                        ( 0, 1 )
                        (Beta.percentiles 0.05 { alpha = 0.002008032128514081, beta = 0.002008032128514081 })
            ]
        , describe "elm vs js"
            [ test "log e 0.5 = -0.6931471805599453" <|
                \() ->
                    equalFloatWithEpsilon eps
                        (-1 * 0.6931471805599453)
                        (logBase e 0.5)
            , test "sqrt (-2 * (log e 0.5)) = 1.1774100225154747" <|
                \() ->
                    equalFloatWithEpsilon eps
                        1.1774100225154747
                        (sqrt (-2 * logBase e 0.5))
            , test "e ^ 3  = 20.085536923187664" <|
                \() ->
                    equalFloatWithEpsilon eps
                        20.085536923187664
                        (e ^ 3)
            ]
        ]
