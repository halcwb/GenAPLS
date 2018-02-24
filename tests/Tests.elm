module Tests exposing (..)

import Expect exposing (Expectation)


--import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)
import Util.ListUtils as List


suite : Test
suite =
    describe "transpose a list of lists"
        [ test "returns the same list if transposed twice" <|
            \() ->
                let
                    testList =
                        [ [ "a", "b", "c" ]
                        , [ "a", "b", "c" ]
                        ]
                in
                    testList
                        |> List.transpose
                        |> List.transpose
                        |> Expect.equal testList
        ]
