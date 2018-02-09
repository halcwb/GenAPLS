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
    |> List.foldl (\x xs ->
        if xs |> List.any (eqs x) then
        xs
        else [x] |> List.append xs
    ) []
