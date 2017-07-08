module Util.ListUtils exposing (..)


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
