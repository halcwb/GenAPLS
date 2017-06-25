module Model.Medication exposing (..)

import FormatNumber exposing (..)
import Util.Locals exposing (..)


type alias Medication =
    { name : String
    , dose : Float
    , dosePerKg : Float
    , max : Float
    , unit : String
    }


medication =
    { name = ""
    , dose = 0
    , dosePerKg = 0
    , max = 0
    , unit = ""
    }


create : ( String, Float, Float, String ) -> Medication
create ( name, dosePerKg, max, unit ) =
    { name = name
    , dose = 0
    , dosePerKg = dosePerKg
    , max = max
    , unit = unit
    }


printDose : Medication -> String
printDose med =
    if med.dose == 0 then
        ""
    else
        (format locale2 med.dose) ++ " " ++ med.unit


print : Medication -> String
print med =
    med.name ++ (format locale2 med.dose) ++ " " ++ med.unit


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
                else
                    d
        }


medicationDefs =
    [ ( "propofol", 2, 0, "mg" )
    , ( "midazolam", 0.2, 10, "mg" )
    , ( "esketamine", 0.5, 5, "mg" )
    , ( "fentanyl", 1, 50, "mcg" )
    , ( "morfine", 0.1, 10, "mg" )
    ]


medicationList =
    medicationDefs |> List.map create
