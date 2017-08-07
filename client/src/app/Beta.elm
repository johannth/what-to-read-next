module Beta exposing (..)


type alias BetaDistributionParameters =
    { alpha : Float
    , beta : Float
    }



-- https://github.com/jstat/jstat/blob/master/src/distribution.js


lnOfGamma : Float -> Float
lnOfGamma x =
    let
        cof : List Float
        cof =
            [ 76.18009172947146
            , -86.50532032941678
            , 24.01409824083091
            , -1.231739572450155
            , 0.001208650973866179
            , -0.000005395239384953
            ]

        -- This is basically 1.000000000190015 + List.sum (List.indexedMap (\j cofJ -> cofJ / (x + toFloat j + 1)) cof) but it causes weird floating point inaccuracies so we fold instead
        ser =
            List.foldl (\( j, cofJ ) ser -> ser + cofJ / (x + toFloat j + 1)) 1.000000000190015 (List.indexedMap (,) cof)

        tmp =
            x + 5.5 - (x + 0.5) * logBase e (x + 5.5)
    in
    logBase e (2.5066282746310007 * ser / x) - tmp


incompleteBeta : BetaDistributionParameters -> Float -> Float
incompleteBeta ({ alpha, beta } as parameters) x =
    let
        bt =
            case x of
                0 ->
                    0

                1 ->
                    0

                _ ->
                    e ^ (lnOfGamma (alpha + beta) - lnOfGamma alpha - lnOfGamma beta + alpha * logBase e x + beta * logBase e (1 - x))
    in
    if x < ((alpha + 1) / (alpha + beta + 2)) then
        bt * betaContinuedFraction parameters x / alpha
    else
        1 - bt * betaContinuedFraction { alpha = beta, beta = alpha } (1 - x) / beta


makeSureIsGreaterThanZero : Float -> Float
makeSureIsGreaterThanZero x =
    if abs x < 1.0e-30 then
        1.0e-30
    else
        x


betaContinuedFraction : BetaDistributionParameters -> Float -> Float
betaContinuedFraction ({ alpha, beta } as parameters) x =
    -- Evaluates the continued fraction for incomplete beta function by modified Lentz's method.
    let
        qab =
            alpha + beta

        qap =
            alpha + 1

        qam =
            alpha - 1

        d0 =
            1 / makeSureIsGreaterThanZero (1 - qab * x / qap)

        ( h, c, d ) =
            betaContinuedFractionRecursion parameters x 1 ( d0, 1, d0 )
    in
    h


betaContinuedFractionRecursion : BetaDistributionParameters -> Float -> Float -> ( Float, Float, Float ) -> ( Float, Float, Float )
betaContinuedFractionRecursion ({ alpha, beta } as parameters) x m ( h, c, d ) =
    if m == 101 then
        ( h, c, d )
    else
        let
            qab =
                alpha + beta

            qap =
                alpha + 1

            qam =
                alpha - 1

            stepOfRecurrence : Float -> ( Float, Float, Float ) -> ( Float, Float, Float )
            stepOfRecurrence aa ( h, c, d ) =
                let
                    dNew =
                        1 / makeSureIsGreaterThanZero (1 + aa * d)

                    cNew =
                        makeSureIsGreaterThanZero (1 + aa / c)

                    hNew =
                        h * dNew * cNew
                in
                ( hNew, cNew, dNew )

            ( hNew, cNew, dNew ) =
                stepOfRecurrence (m * (beta - m) * x / ((qam + 2 * m) * (alpha + 2 * m))) ( h, c, d )
                    |> stepOfRecurrence (-1 * (alpha + m) * (qab + m) * x / ((alpha + 2 * m) * (qap + 2 * m)))
        in
        if abs (d * c - 1) < 3.0e-7 then
            ( h, c, d )
        else
            betaContinuedFractionRecursion parameters x (m + 1) ( hNew, cNew, dNew )


maximumVarianceFromMean : Float -> Float
maximumVarianceFromMean mean =
    mean * (1 - mean) - 1.0e-12


estimateBetaDistributionParameters : Float -> Float -> BetaDistributionParameters
estimateBetaDistributionParameters mean variance =
    -- mean in (0, 1)
    -- variance in (0, maximumVarianceFromMean) < (0, 0.5^2)
    let
        adjustedMean =
            case mean of
                0.0 ->
                    0.01

                1.0 ->
                    0.99

                _ ->
                    mean

        adjustedVariance =
            case variance of
                0 ->
                    maximumVarianceFromMean adjustedMean

                _ ->
                    variance

        alpha =
            ((1 - adjustedMean) / adjustedVariance - 1 / adjustedMean) * adjustedMean ^ 2

        beta =
            alpha * (1 / adjustedMean - 1)
    in
    { alpha = alpha, beta = beta }


percentiles : Float -> BetaDistributionParameters -> ( Float, Float )
percentiles percent parameters =
    ( inverseCDF parameters percent, inverseCDF parameters (1 - percent) )


inverseCDF : BetaDistributionParameters -> Float -> Float
inverseCDF ({ alpha, beta } as parameters) p =
    if p <= 0 then
        0
    else if p >= 1 then
        1
    else
        let
            x0 =
                if alpha >= 1 && beta >= 1 then
                    let
                        pp =
                            if p < 0.5 then
                                p
                            else
                                1 - p

                        t =
                            sqrt (-2 * logBase e pp)

                        y =
                            (if p < 0.5 then
                                -1
                             else
                                1
                            )
                                * ((2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t)

                        al =
                            (y * y - 3) / 6

                        h =
                            2 / (1 / (2 * alpha - 1) + 1 / (2 * beta - 1))

                        w =
                            (y * sqrt (al + h) / h) - (1 / (2 * beta - 1) - 1 / (2 * alpha - 1)) * (al + 5 / 6 - 2 / (3 * h))
                    in
                    alpha / (alpha + beta * e ^ (2 * w))
                else
                    let
                        lna =
                            logBase e (alpha / (alpha + beta))

                        lnb =
                            logBase e (beta / (alpha + beta))

                        t =
                            e ^ (alpha * lna) / alpha

                        u =
                            e ^ (beta * lnb) / beta

                        w =
                            t + u
                    in
                    if p < (t / w) then
                        (alpha * w * p) ^ (1 / alpha)
                    else
                        1 - (beta * w * (1 - p)) ^ (1 / beta)

            afac =
                -1 * lnOfGamma alpha - lnOfGamma beta + lnOfGamma (alpha + beta)
        in
        inverseCDFRecursion parameters afac p 0 x0


inverseCDFRecursion : BetaDistributionParameters -> Float -> Float -> Float -> Float -> Float
inverseCDFRecursion ({ alpha, beta } as parameters) afac p j x =
    if j == 10 then
        x
    else
        let
            scaleBy : (Float -> Bool) -> (Float -> Float) -> Float -> Float
            scaleBy condition scaler value =
                if condition value then
                    scaler value
                else
                    value
        in
        case x of
            0 ->
                0

            1 ->
                1

            _ ->
                let
                    err =
                        incompleteBeta parameters x - p

                    t =
                        e ^ ((alpha - 1) * logBase e x + (beta - 1) * logBase e (1 - x) + afac)

                    u =
                        err / t

                    tNew =
                        u / (1 - 0.5 * min 1 (u * ((alpha - 1) / x - (beta - 1) / (1 - x))))

                    xNew =
                        x - tNew

                    xFinal =
                        scaleBy (\x -> x <= 0) (\x -> 0.5 * (x + tNew)) xNew
                            |> scaleBy (\x -> x >= 1) (\x -> 0.5 * (x + tNew + 1))
                in
                if abs t < 1.0e-8 * xFinal && j > 0 then
                    xFinal
                else
                    inverseCDFRecursion parameters afac p (j + 1) xFinal
