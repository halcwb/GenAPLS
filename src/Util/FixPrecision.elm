module Util.FixPrecision exposing (fixPrecision)

import FormatNumber exposing (..)
import Util.Locals exposing (..)


countFirstInString : Char -> String -> Int
countFirstInString c s =
    let
        ( n, _ ) =
            s
                |> String.foldl
                    (\c_ n0 ->
                        let
                            ( n, is0 ) =
                                n0
                        in
                            if is0 && c_ == c then
                                ( n + 1, True )
                            else
                                ( n, False )
                    )
                    ( 0, True )
    in
        Debug.log "countFirstInString"
            n


getPower : Float -> Int -> Int
getPower num prec =
    let
        ( n, d ) =
            case toString num |> String.split "." of
                [ numer, denom ] ->
                    ( numer, denom )

                [ numer ] ->
                    ( numer, "" )

                _ ->
                    ( "", "" )

        l =
            if n == "0" then
                0
            else
                String.length n

        p =
            if prec - l < 0 then
                0
            else
                prec - l
    in
        Debug.log ("GetPower" ++ " n: " ++ n ++ " d: " ++ d)
            (if d == "" || num > 1 then
                p
             else
                p + countFirstInString '0' d
            )


fixPrecision : Float -> Int -> String
fixPrecision num prec =
    format (local_ (getPower num prec)) num
