module Page.EmergencyList exposing (..)

import FormatNumber exposing (format)
import Navigation
import Dict exposing (Dict)
import Http


-- import Component.CheckMenu as CheckMenu

import Util.FixPrecision as US exposing (fixPrecision)
import Util.FloatUtils exposing (roundBy, calcDoseVol)
import Util.Locals exposing (..)
import Util.ListUtils exposing (findNearestMax, removeDuplicates)
import Data.Medication as Medication
import Util.Utils exposing (eqs)


-- Constants


no_age : Float
no_age =
    -0.5


zero_age : number
zero_age =
    0


half_age : Float
half_age =
    0.5


min_weight : number
min_weight =
    3


max_weight : number
max_weight =
    150


max_defib : number
max_defib =
    150


max_epi : number
max_epi =
    1


joules : List number
joules =
    [ 1
    , 2
    , 3
    , 5
    , 7
    , 10
    , 20
    , 30
    , 50
    , 70
    , 100
    , 150
    ]



-- Model


type Age
    = Year
    | Month


type Calculated
    = IsCalc
    | NotCalc


type alias Model =
    { age : Float
    , year : Float
    , month : Float
    , ageText : String
    , weight : Float
    , weightText : String
    , calcWeight : Bool
    , tubeSize : ( Float, Float, Float )
    , tubeLengthOral : Float
    , tubeLengthNasal : Float
    , epinephrineIV : ( Float, Float, Float )
    , epinephrineTR : ( Float, Float, Float )
    , fluidBolus : Float
    , defibrillation : Float
    , cardioversion : Float
    , medications : List Medication.Bolus
    , calculated : Calculated
    , indicatieSelect : SelectModel
    }


type alias SelectModel =
    { indications : List String
    , selections : List String
    , all : String
    }


selectModel : SelectModel
selectModel =
    let
        inds =
            Medication.medicationList
                |> List.map .category
                |> removeDuplicates

        sels =
            []

        all =
            "alles"
    in
        SelectModel inds sels all


model : Model
model =
    { age = no_age
    , year = 0
    , month = 0
    , ageText = ""
    , weight = 0
    , weightText = ""
    , calcWeight = True
    , tubeSize = ( 0, 0, 0 )
    , tubeLengthOral = 0
    , tubeLengthNasal = 0
    , epinephrineIV = ( 0, 0, 0 )
    , epinephrineTR = ( 0, 0, 0 )
    , fluidBolus = 0
    , defibrillation = 0
    , cardioversion = 0
    , medications = Medication.medicationList
    , calculated = NotCalc
    , indicatieSelect = selectModel
    }


init : Navigation.Location -> Model
init location =
    let
        years =
            case
                location.search
                    |> parseParams
                    |> Dict.get "years"
            of
                Just a ->
                    a

                Nothing ->
                    ""

        months =
            case
                location.search
                    |> parseParams
                    |> Dict.get "months"
            of
                Just a ->
                    a

                Nothing ->
                    ""

        wght =
            case
                location.search
                    |> parseParams
                    |> Dict.get "weight"
            of
                Just w ->
                    w

                Nothing ->
                    ""

        initModel =
            if wght == "" then
                model
                    |> setAge Year years
                    |> setAge Month months
            else
                model
                    |> setAge Year years
                    |> setAge Month months
                    |> setWeight wght
    in
        initModel |> calculate



-- Setters


setAge : Age -> String -> Model -> Model
setAge a age model =
    case String.toFloat age of
        Ok n ->
            if n >= zero_age then
                case a of
                    Year ->
                        { model | age = n + model.month / 12, year = n, calcWeight = True }

                    Month ->
                        { model | age = model.year + n / 12, month = n, calcWeight = True }
            else
                model

        Err _ ->
            model


setWeight : String -> Model -> Model
setWeight txt model =
    let
        model_ =
            case String.toFloat txt of
                Ok n ->
                    if n >= min_weight && n <= max_weight then
                        { model | weight = n, weightText = txt, calcWeight = False }
                    else
                        { model
                            | weight = 0
                            , weightText = txt
                            , calcWeight = False
                        }

                Err _ ->
                    if model.weight == 0 || txt == "" then
                        { model
                            | weight = 0
                            , weightText = ""
                            , calcWeight = False
                        }
                    else
                        model
    in
        model_



-- Calculate


calcMinMax : Float -> Float -> (Float -> Float) -> Float -> Float
calcMinMax min max f x =
    let
        y =
            f x
    in
        if y > max then
            max
        else if y < min then
            min
        else
            y


calcMax : Float -> (Float -> Float) -> Float -> Float
calcMax =
    calcMinMax 0


calcAge : Model -> Model
calcAge model =
    if model.age == no_age then
        model
    else
        { model | ageText = format locale1 model.age ++ " jaar" }


calcWeight : Model -> Model
calcWeight model =
    if model.calcWeight then
        let
            age_zero_weight =
                3.5

            age_6mo_weight =
                6
        in
            if model.age == no_age then
                model
            else if model.age == zero_age then
                { model | weight = age_zero_weight, weightText = toString age_zero_weight }
            else if model.age > zero_age && model.age <= half_age then
                { model | weight = age_6mo_weight, weightText = toString age_6mo_weight }
            else
                let
                    w =
                        model.age * 2.5 + 8
                in
                    { model | weight = w, weightText = toString w }
    else
        model


calcTubeSize : Model -> Model
calcTubeSize model =
    let
        maxSize =
            7.5

        calc a =
            let
                c =
                    a |> calcMax maxSize (\n -> ((n / 4) + 4) |> roundBy 0.5)

                l =
                    c - 0.5

                r =
                    c + 0.5
            in
                ( l, c, r )
    in
        if model.age == zero_age then
            { model | tubeSize = ( 3.0, 3.5, 4.0 ) }
        else
            { model | tubeSize = calc model.age }


calcTubLength : Float -> Model -> Model
calcTubLength n model =
    let
        l =
            model.age
                / 2
                + n
                |> roundBy 0.5
    in
        if n == 12 then
            { model | tubeLengthOral = l }
        else
            { model | tubeLengthNasal = l }


calcTubeLengthOral : Model -> Model
calcTubeLengthOral =
    calcTubLength 12


calcTubeLengthNasal : Model -> Model
calcTubeLengthNasal model =
    calcTubLength 15 model


calcEpinephrine : Model -> Model
calcEpinephrine model =
    let
        iv =
            calcDoseVol model.weight 0.01 0.1 0.01 0.5

        tr =
            calcDoseVol model.weight 0.1 0.1 0.1 5
    in
        Debug.log "calcEpi"
            { model
                | epinephrineIV = ( iv |> Tuple.first, iv |> Tuple.second, (iv |> Tuple.second) / 10 )
                , epinephrineTR = ( tr |> Tuple.first, tr |> Tuple.second, (tr |> Tuple.second) / 10 )
            }


calcFluidBolus : Model -> Model
calcFluidBolus model =
    { model | fluidBolus = calcMax 1000 (\n -> (n * 20) |> roundBy 10) model.weight }


calcDefib : Model -> Model
calcDefib model =
    let
        d =
            joules
                |> findNearestMax (model.weight * 4)
    in
        { model
            | defibrillation =
                if d > max_defib then
                    max_defib
                else
                    d
        }


calcCardiov : Model -> Model
calcCardiov model =
    let
        d =
            joules
                |> findNearestMax (model.weight * 2)
    in
        { model
            | cardioversion =
                if d > max_defib then
                    max_defib
                else
                    d
        }


calcMeds : Model -> Model
calcMeds model =
    { model
        | medications =
            model.medications
                |> List.map (Medication.calculate model.weight)
    }


setCalculated : Bool -> Model -> Model
setCalculated b model =
    { model
        | calculated =
            if b then
                IsCalc
            else
                NotCalc
    }


isCalculated : Model -> Bool
isCalculated model =
    case model.calculated of
        IsCalc ->
            True

        NotCalc ->
            False


calculate : Model -> Model
calculate mdl =
    if mdl.age == no_age then
        model
            |> (setCalculated False)
    else if mdl.weight == 0 && not mdl.calcWeight then
        mdl
    else
        mdl
            |> calcWeight
            |> calcTubeSize
            |> calcAge
            |> calcTubeLengthNasal
            |> calcTubeLengthOral
            |> calcEpinephrine
            |> calcFluidBolus
            |> calcDefib
            |> calcCardiov
            |> calcMeds
            |> (setCalculated True)



-- Print


type alias Intervention =
    { indication : String
    , intervention : String
    , value : String
    , preparation : String
    , solution : String
    , advice : String
    }


printEmergencyList : Model -> List Intervention
printEmergencyList model =
    let
        meds =
            let
                dosePerKg m =
                    m.dose
                        / model.weight
                        |> fixPrecision 1

                dose m =
                    (m |> Medication.printDoseVolume >> Tuple.first)
                        ++ " ("
                        ++ dosePerKg m
                        ++ " "
                        ++ m.unit
                        ++ "/kg)"

                volume m =
                    m |> Medication.printDoseVolume >> Tuple.second

                advice m =
                    m |> Medication.printAdvice
            in
                List.map (\m -> Intervention m.category m.name (m |> dose) (m |> volume) "" (m |> advice)) model.medications
    in
        ([ Intervention "reanimatie" "tube maat" (printTubeSize model) "" "" "4 + Leeftijd / 4"
         , Intervention "reanimatie" "tube lengte oraal" (printTubeLengthOral model) "" "" "12 + Leeftijd / 2"
         , Intervention "reanimatie" "tube lengte nasaal" (printTubeLengthNasal model) "" "" "15 + Leeftijd / 2"
         , Intervention "reanimatie" "epinephrine iv/io" (model |> printEpinephrineIV >> Tuple.first) (model |> printEpinephrineIV >> Tuple.second) "" "0,01 mg/kg iv"
         , Intervention "reanimatie" "epinephrine tracheaal" (model |> printEpinephrineTR >> Tuple.first) (model |> printEpinephrineTR >> Tuple.second) "" "0,1 mg/kg trach"
         , Intervention "reanimatie" "vaat vulling" (model |> printFluidBolus >> Tuple.first) (model |> printFluidBolus >> Tuple.second) "" "20 ml/kg"
         , Intervention "reanimatie" "defibrillatie" (model |> printDefibrillation >> Tuple.first) (model |> printDefibrillation >> Tuple.second) "" "4 Joule/kg"
         , Intervention "reanimatie" "cardioversie" (model |> printCardioversion |> Tuple.first) (model |> printCardioversion >> Tuple.second) "" "2 Joule/kg"
         ]
            ++ meds
        )
            |> List.filter (\m -> (model.indicatieSelect.selections |> List.isEmpty) || (model.indicatieSelect.selections |> List.any ((==) m.indication)))


printAge : Model -> String
printAge model =
    if model.age == no_age then
        ""
    else
        format locale1 model.age ++ " jaar"


printWeight : Model -> String
printWeight model =
    if model.weight == 0 then
        ""
    else
        format locale1 model.weight ++ " kg"


printTubeSize : Model -> String
printTubeSize model =
    if model.tubeSize == ( 0, 0, 0 ) then
        ""
    else
        let
            print n =
                format locale1 n

            ( r, c, l ) =
                model.tubeSize
        in
            (r |> print)
                ++ " - "
                ++ (c |> print)
                ++ " - "
                ++ (l |> print)


printTubeLength : Float -> String
printTubeLength n =
    if n == 0 then
        ""
    else
        format locale1 n ++ " cm"


printTubeLengthOral : Model -> String
printTubeLengthOral model =
    printTubeLength model.tubeLengthOral


printTubeLengthNasal : Model -> String
printTubeLengthNasal model =
    printTubeLength model.tubeLengthNasal


printEpinephrine : Float -> ( Float, Float, Float ) -> String -> ( String, String )
printEpinephrine w e r =
    if e == ( 0, 0, 0 ) then
        ( "", "" )
    else
        let
            ( d, s1, s2 ) =
                e

            dosePerKg =
                d / w |> fixPrecision 2
        in
            ( (US.fixPrecision 2 d ++ " mg" ++ " " ++ r ++ "(" ++ dosePerKg ++ " mg/kg)")
            , (Util.FloatUtils.printVolume s1 ++ " ml van 0,1 mg/ml (1:10.000) of ")
                ++ (Util.FloatUtils.printVolume s2 ++ " ml van 1 mg/ml (1:1000)")
            )


printEpinephrineIV : Model -> ( String, String )
printEpinephrineIV model =
    printEpinephrine model.weight model.epinephrineIV ""


printEpinephrineTR : Model -> ( String, String )
printEpinephrineTR model =
    printEpinephrine model.weight model.epinephrineTR ""


printFluidBolus : Model -> ( String, String )
printFluidBolus model =
    if model.fluidBolus == 0 then
        ( "", "" )
    else
        ( format locale0 model.fluidBolus ++ " ml NaCl 0,9%", "" )


printDefibrillation : Model -> ( String, String )
printDefibrillation model =
    if model.defibrillation == 0 then
        ( "", "" )
    else
        ( format locale0 model.defibrillation ++ " Joule", "" )


printCardioversion : Model -> ( String, String )
printCardioversion model =
    if model.cardioversion == 0 then
        ( "", "" )
    else
        ( format locale0 model.cardioversion ++ " Joule", "" )



--- UrlParser


parseParams : String -> Dict String String
parseParams queryString =
    queryString
        |> String.dropLeft 1
        |> String.split "&"
        |> List.filterMap toKeyValuePair
        |> Dict.fromList


toKeyValuePair : String -> Maybe ( String, String )
toKeyValuePair segment =
    case String.split "=" segment of
        [ key, value ] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

        _ ->
            Nothing



-- Update


update : String -> Model -> Model
update s model =
    let
        indSel =
            model.indicatieSelect

        indSel_ =
            if s == indSel.all then
                { indSel | selections = [] }
            else if indSel.selections |> List.any (eqs s) then
                { indSel | selections = indSel.selections |> List.filter (\x -> not (x == s)) }
            else
                { indSel | selections = indSel.selections |> List.append [ s ] }
    in
        { model | indicatieSelect = indSel_ }
