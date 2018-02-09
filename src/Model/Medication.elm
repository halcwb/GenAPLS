module Model.Medication exposing (..)

import FormatNumber exposing (..)
import String.Extra exposing (replace)
import Util.FixPrecision exposing (fixPrecision)
import Util.Locals exposing (..)
import Util.FloatUtils exposing (roundBy, calcDoseVol, printVolume)


type alias Bolus =
    { category : String
    , name : String
    , dose : Float
    , dosePerKg : Float
    , min : Float
    , max : Float
    , conc : Float
    , volume : Float
    , unit : String
    , remark : String
    }


medication : Bolus
medication =
    { category = ""
    , name = ""
    , dose = 0
    , dosePerKg = 0
    , min = 0
    , max = 0
    , conc = 0
    , volume = 0
    , unit = ""
    , remark = ""
    }


create : ( String, String, Float, Float, Float, Float, String, String ) -> Bolus
create ( cat, name, dosePerKg, min, max, conc, unit, rem ) =
    { category = cat
    , name = name
    , dose = 0
    , dosePerKg = dosePerKg
    , min = min
    , max = max
    , conc = conc
    , volume = 0
    , unit = unit
    , remark = rem
    }


printVolume : Bolus -> String
printVolume med =
      if med.volume == 0 then
          ""
      else
           Util.FloatUtils.printVolume med.volume
              ++ " "
              ++ "ml"


printDose : Bolus -> String
printDose med =
    if med.dose == 0 then
        ""
    else
        fixPrecision 2 med.dose ++ " " ++ med.unit


printDoseVolume : Bolus -> ( String, String )
printDoseVolume med =
    let
        d =
            printDose med

        v =
            printVolume med
    in
        if d == "" then
            ( "", "" )
        else
            ( d
            , (if v == "" then
                ""
               else
                printVolume med
                    ++ " van "
                    ++ (toString med.conc |> replace "." ",")
                    ++ " "
                    ++ med.unit
                    ++ "/ml"
              )
            )


print : Bolus -> String
print med =
    med.category
        ++ " "
        ++ med.name
        ++ format locale2 med.dose
        ++ " "
        ++ med.unit


calculate : Float -> Bolus -> Bolus
calculate kg med =
    let
        ( d, v ) = calcDoseVol kg med.dosePerKg med.conc med.min med.max
    in
        { med
            | dose = d
            , volume = v |> roundBy 0.1
        }



medicationDefs :
    List ( String, String, Float, Float, Float, Float, String, String )
medicationDefs =
    [ ( "reanimatie", "glucose 10%", 0.2, 0, 25, 0.1, "gram", "" )
    , ( "reanimatie", "NaBic 8,4", 0.5, 0, 50, 1, "mmol", "" )
    , ( "intubatie", "propofol", 2, 0, 0, 10, "mg", "" )
    , ( "intubatie", "midazolam", 0.2, 0, 10, 5, "mg", "" )
    , ( "intubatie", "esketamine", 0.5, 0, 5, 5, "mg", "" )
    , ( "intubatie", "etomidaat", 0.5, 0, 20, 2, "mg", "" )
    , ( "intubatie", "fentanyl", 1, 0, 50, 50, "mcg", "" )
    , ( "intubatie", "morfine", 0.1, 0, 10, 1, "mg", "" )
    , ( "intubatie", "rocuronium", 1, 0, 10, 10, "mg", "" )
    , ( "intubatie", "atropine", 0.02, 0.1, 0.5, 0.5, "mg", "" )
    , ( "antidota", "flumazine", 0.02, 0, 0.3, 0.1, "mg", "" )
    , ( "antidota", "naloxon", 0.01, 0, 0.5, 0.02, "mg", "" )
    , ( "antiarrythmica", "amiodarone", 5, 0, 300, 50, "mg", "" )
    , ( "antiarrythmica", "calciumchloride 10%", 0.12, 0, 5, 0.68, "mmol", "" )
    , ( "anticonvulsiva", "diazepam", 0.5, 0, 10, 2, "mg", "" )
    , ( "anticonvulsiva", "fenytoine", 20, 0, 1500, 50, "mg", "" )
    , ( "anticonvulsiva", "midazolam", 0.1, 0, 10, 5, "mg", "" )
    , ( "diversen", "prednisolon", 1, 0, 25, 12.5, "mg", "" )
    , ( "diversen", "mannitol 15%", 0.5, 0, 50, 0.15, "gram", "" )
    ]


medicationList : List Bolus
medicationList =
    medicationDefs |> List.map create








