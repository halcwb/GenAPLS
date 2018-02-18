module Util.ListUtils exposing (..)

import Util.Utils exposing (eqs)


findNearestMax : Float -> List Float -> Float
findNearestMax n ns =
    case ns of
        [] ->
            n

        _ ->
            ns
                |> List.sort
                |> List.reverse
                |> List.foldl
                    (\a x ->
                        if (a - x) < (n - x) then
                            x
                        else
                            a
                    )
                    n


removeDuplicates : List a -> List a
removeDuplicates xs =
    xs
        |> List.foldl
            (\x xs ->
                if xs |> List.any (eqs x) then
                    xs
                else
                    [ x ] |> List.append xs
            )
            []


mapi : (a -> Int -> b) -> List a -> List b
mapi f xs =
    let
        mapi_ i xs_ map_ =
            case xs_ of
                [] ->
                    map_

                head :: tail ->
                    f head i :: (map_ |> mapi_ (i + 1) tail)
    in
        mapi_ 0 xs []
