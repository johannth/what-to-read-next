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
        []
