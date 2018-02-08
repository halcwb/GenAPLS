module Util.FloatUtils exposing (roundBy, roundBy0_5, calcDoseVol)


roundBy : Float -> Float -> Float
roundBy s n =
    (n / s)
        |> round
        |> toFloat
        |> (\f -> f * s)


roundBy0_5 : Float -> Float
roundBy0_5 =
    roundBy 0.5


calcDoseVol : Float -> Float -> Float -> Float -> Float -> ( Float, Float )
calcDoseVol kg dosePerKg conc min max =
    let
        d =
            kg * dosePerKg

        d_ =
            if max > 0 && d > max then
                max
            else if min > 0 && d < min then
                min
            else
                d

        v =
            d / conc

        v_ =
            if v >= 10 then
                v |> roundBy 1
            else
                v |> roundBy 0.1
    in
        ( v_ * conc, v_ )
