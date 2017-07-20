module TestUtils exposing (..)

import Expect


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
