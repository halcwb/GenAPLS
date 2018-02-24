module Tests exposing (..)

import Expect exposing (Expectation)


--import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)
import Util.ListUtils as List


listIsRagged : Test
listIsRagged =
    describe "list is ragged returns"
        [ test "False when empty" <|
            \() ->
                []
                    |> List.isRagged
                    |> Expect.equal False
        , test "False when only one list" <|
            \() ->
                [ [ "a" ] ]
                    |> List.isRagged
                    |> Expect.equal False
        , test "True when two lists with different length" <|
            \() ->
                [ [ "a" ], [ "a", "b" ] ]
                    |> List.isRagged
                    |> Expect.equal True
        ]


(>=>) : Result a b -> (b -> Result a c) -> Result a c
(>=>) result f =
    case result of
        Ok r ->
            r |> f

        Err s ->
            s |> Err
infixl 0 >=>


equalOk : a -> Result String a -> Expectation
equalOk a result =
    case result of
        Ok r ->
            r |> Expect.equal a

        Err e ->
            e |> Expect.fail


listTranspose : Test
listTranspose =
    describe "transposing a list of lists"
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
                        >=> List.transpose
                        |> equalOk testList
        , test "an empty list transposes to an empty list" <|
            \() ->
                let
                    testList =
                        []
                in
                    testList
                        |> List.transpose
                        >=> List.transpose
                        |> equalOk []
        , test "an list with an empty list transposes to an list with an empty list" <|
            \() ->
                let
                    testList =
                        [ [] ]
                in
                    testList
                        |> List.transpose
                        >=> List.transpose
                        |> equalOk [ [] ]
        , test "will crash when trying to transpose a ragged list" <|
            \() ->
                let
                    testList =
                        [ [ "a" ], [ "a", "b" ] ]
                in
                    testList
                        |> List.transpose
                        >=> List.transpose
                        |> Expect.err
        ]
