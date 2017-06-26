module Model.Medication exposing (..)

import FormatNumber exposing (..)
import String.Extra exposing (replace)
import Util.FixPrecision exposing (fixPrecision)
import Util.Locals exposing (..)


type alias Medication =
    { name : String
    , dose : Float
    , dosePerKg : Float
    , min : Float
    , max : Float
    , conc : Float
    , volume : Float
    , unit : String
    }


medication =
    { name = ""
    , dose = 0
    , dosePerKg = 0
    , min = 0
    , max = 0
    , conc = 0
    , volume = 0
    , unit = ""
    }


create : ( String, Float, Float, Float, Float, String ) -> Medication
create ( name, dosePerKg, min, max, conc, unit ) =
    { name = name
    , dose = 0
    , dosePerKg = dosePerKg
    , min = min
    , max = max
    , conc = conc
    , volume = 0
    , unit = unit
    }


printVolume : Medication -> String
printVolume med =
    if med.volume == 0 then
        ""
    else
        fixPrecision med.volume 1
            ++ " "
            ++ "ml"


printDose : Medication -> String
printDose med =
    if med.dose == 0 then
        ""
    else
        fixPrecision med.dose 1 ++ " " ++ med.unit


printDoseVolume : Medication -> String
printDoseVolume med =
    let
        d =
            printDose med

        v =
            printVolume med
    in
    if d == "" then
        ""
    else
        d
            ++ (if v == "" then
                    ""
                else
                    " = "
                        ++ printVolume med
                        ++ " van "
                        ++ (toString med.conc |> replace "." ",")
                        ++ " "
                        ++ med.unit
                        ++ "/ml"
               )


print : Medication -> String
print med =
    med.name
        ++ format locale2 med.dose
        ++ " "
        ++ med.unit


calculate : Float -> Medication -> Medication
calculate kg med =
    let
        d =
            kg * med.dosePerKg
    in
    { med
        | dose =
            if med.max > 0 && d > med.max then
                med.max
            else if med.min > 0 && d < med.min then
                med.min
            else
                d
        , volume =
            if med.conc > 0 then
                d / med.conc
            else
                0
    }


medicationDefs =
    [ ( "glucose 10%", 0.2, 0, 25, 0.1, "gram" )
    , ( "NaBic 8,4", 0.5, 0, 50, 1, "mmol" )
    , ( "propofol", 2, 0, 0, 10, "mg" )
    , ( "midazolam", 0.2, 0, 10, 5, "mg" )
    , ( "esketamine", 0.5, 0, 5, 5, "mg" )
    , ( "etomidaat", 0.5, 0, 20, 2, "mg" )
    , ( "fentanyl", 1, 0, 50, 50, "mcg" )
    , ( "morfine", 0.1, 0, 10, 1, "mg" )
    , ( "rocuronium", 1, 0, 10, 10, "mg" )
    , ( "atropine", 0.02, 0.1, 0.5, 0.5, "mg" )
    , ( "flumazine", 0.02, 0, 0.3, 0.1, "mg" )
    , ( "naloxon", 0.01, 0, 0.5, 0.02, "mg" )
    , ( "amiodarone", 5, 0, 300, 50, "mg" )
    , ( "calciumchloride 10%", 0.12, 0, 5, 0.68, "mmol" )
    , ( "diazepam", 0.5, 0, 10, 2, "mg" )
    , ( "fenytoine", 20, 0, 1500, 50, "mg" )
    , ( "midazolam", 0.1, 0, 10, 5, "mg" )
    , ( "prednisolon", 1, 0, 25, 0, "mg" )
    , ( "mannitol 15%", 0.5, 0, 50, 0.15, "gram" )
    ]


medicationList =
    medicationDefs |> List.map create
