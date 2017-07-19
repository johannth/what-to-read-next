module Beta exposing (..)


type alias BetaDistributionParameters =
    { alpha : Float
    , beta : Float
    }



-- https://github.com/jstat/jstat/blob/master/src/distribution.js


lnOfGamma : Float -> Float
lnOfGamma x =
    let
        cof =
            [ 76.18009172947146
            , -86.50532032941678
            , 24.01409824083091
            , -1.231739572450155
            , 1.208650973866179e-3
            , -5.395239384953e-6
            ]
    in
    x


estimateBetaDistributionParameters : Float -> Float -> BetaDistributionParameters
estimateBetaDistributionParameters mean variance =
    -- mean in [0, 1]
    -- variance in []
    case variance of
        0 ->
            { alpha = 1, beta = 1 }

        _ ->
            let
                alpha =
                    ((1 - mean) / variance - 1 / mean) * mean ^ 2

                beta =
                    alpha * (1 / mean - 1)
            in
            { alpha = alpha, beta = beta }


percentiles : Float -> BetaDistributionParameters -> ( Float, Float )
percentiles percent parameters =
    ( 0.1, 0.9 )


inverseCDF : Float -> BetaDistributionParameters -> Float
inverseCDF p { alpha, beta } =
    if p <= 0 then
        0
    else if p >= 1 then
        1
    else
        let
            j =
                0

            --TODO
            afac =
                3

            x =
                0
        in
        x



-- https://github.com/jstat/jstat/blob/bbb79875c0708c1687e9f8082c63350455130e0e/src/special.js
-- https://github.com/jstat/jstat/blob/master/src/distribution.js
-- convergeOnBeta : BetaDistributionParameters -> Float -> Float -> Float
-- convergeOnBeta ({ alpha, beta } as parameters) afac j x =
--     if j == 10 then
--         x
--     else
--         case x of
--             0 ->
--                 0
--
--             1 ->
--                 1
--
--             _ ->
--                 let
--                     EPS =
--                         1.0e-8
--
--                     err =
--                         4
--
--                     -- TODO
--                     a1 =
--                         alpha -1
--
--                     b1 =
--                         beta - 1
--
--                     t =
--                         exp (a1 * logBase 10 x + b1 * logBase (1 -x) + afac)
--
--                     u =
--                         err / t
--
--                     x_i = x -
--                 in
--                 convergeOnBeta parameters afac (j + 1) x
