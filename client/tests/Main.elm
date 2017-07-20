port module Main exposing (..)

import BetaTests
import Json.Encode exposing (Value)
import Test
import Test.Runner.Node exposing (TestProgram, run)
import Tests


main : TestProgram
main =
    run emit (Test.concat [ Tests.all, BetaTests.all ])


port emit : ( String, Value ) -> Cmd msg
