module Util.Float exposing (roundBy, roundBy0_5)


roundBy : Float -> Float -> Float
roundBy s n =
    (s * n * 10)
        |> ceiling
        |> toFloat
        |> (\f -> f / (s * 10))
        |> ceiling
        |> toFloat


roundBy0_5 =
    roundBy 0.5
