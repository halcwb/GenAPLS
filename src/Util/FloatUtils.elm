module Util.FloatUtils exposing (roundBy, roundBy0_5)


roundBy : Float -> Float -> Float
roundBy s n =
    (n / s)
        |> round
        |> toFloat
        |> (\f -> f * s)


roundBy0_5 =
    roundBy 0.5
