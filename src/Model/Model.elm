module Model.Model exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Util.Float exposing (roundBy)


-- Constants


no_age =
    -0.5


zero_age =
    0


half_age =
    0.5



-- Model


type alias Model =
    { age : Float
    , ageText : String
    , weight : Float
    , tubeSize : ( Float, Float, Float )
    , tubeLengthOral : Float
    , tubeLengthNasal : Float
    , epinephrineIV : ( Float, Float, Float )
    , epinephrineTR : ( Float, Float, Float )
    , fluidBolus : Float
    , defibrillation : Float
    , cardioversion : Float
    }


model =
    { age = no_age
    , ageText = ""
    , weight = 0
    , tubeSize = ( 0, 0, 0 )
    , tubeLengthOral = 0
    , tubeLengthNasal = 0
    , epinephrineIV = ( 0, 0, 0 )
    , epinephrineTR = ( 0, 0, 0 )
    , fluidBolus = 0
    , defibrillation = 0
    , cardioversion = 0
    }



-- Setters


setAge : String -> Model -> Model
setAge age model =
    let
        n =
            case String.toFloat age of
                Ok n ->
                    if n >= zero_age then
                        if model.age == no_age then
                            0.5
                        else
                            n
                    else
                        no_age

                Err _ ->
                    model.age

        updated_model =
            { model | age = n }
    in
        updated_model



-- Calculate


calcAge : Model -> Model
calcAge model =
    if model.age == no_age then
        model
    else
        { model | ageText = (format (Locale 1 "," ".") model.age) ++ " jaar" }


calcWeight : Model -> Model
calcWeight model =
    let
        age_zero_weight =
            3.5

        age_6mo_weight =
            6
    in
        if model.age == no_age then
            model
        else if model.age == zero_age then
            { model | weight = age_zero_weight }
        else if model.age > zero_age && model.age <= half_age then
            { model | weight = age_6mo_weight }
        else
            { model | weight = model.age * 2.5 + 8 }


calcTubeSize : Model -> Model
calcTubeSize model =
    let
        calc a =
            let
                c =
                    ((a / 4)
                        + 4
                    )
                        |> roundBy 0.5

                l =
                    c - 0.5

                r =
                    c + 0.5
            in
                ( l, c, r )
    in
        if model.age < 1 then
            { model | tubeSize = ( 3.5, 4.0, 4.5 ) }
        else
            { model | tubeSize = calc model.age }


calcTubLength : Float -> Model -> Model
calcTubLength n model =
    let
        l =
            model.age / 2 + n
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
            0.01

        tr =
            0.1
    in
        { model
            | epinephrineIV = ( iv * model.weight, (iv / 1) * model.weight, (iv / 10) * model.weight )
            , epinephrineTR = ( tr * model.weight, (tr / 1) * model.weight, (tr / 10) * model.weight )
        }


calcFluidBolus : Model -> Model
calcFluidBolus model =
    { model | fluidBolus = model.weight * 20 }


calcDefib : Model -> Model
calcDefib model =
    { model | defibrillation = model.weight * 4 }


calcCardiov : Model -> Model
calcCardiov model =
    { model | cardioversion = model.weight * 2 }


calculate : Model -> Model
calculate model =
    model
        |> calcWeight
        |> calcTubeSize
        |> calcAge
        |> calcTubeLengthNasal
        |> calcTubeLengthOral
        |> calcEpinephrine
        |> calcFluidBolus
        |> calcDefib
        |> calcCardiov



-- Print


locale1 =
    Locale 1 "." ","


locale2 =
    Locale 2 "." ","


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


printEpinephrine : ( Float, Float, Float ) -> String -> String
printEpinephrine e r =
    if e == ( 0, 0, 0 ) then
        ""
    else
        let
            ( d, s1, s2 ) =
                e
        in
            (format locale2 d ++ " mg" ++ " " ++ r ++ " = ")
                ++ (format locale2 s1 ++ " ml van 0,1 mg/ml = 1:10.0000 oplossing of ")
                ++ (format locale2 s2 ++ " ml van 1 mg/ml = 1:10000 oplossing")


printEpinephrineIV : Model -> String
printEpinephrineIV model =
    printEpinephrine model.epinephrineIV "iv"


printEpinephrineTR : Model -> String
printEpinephrineTR model =
    printEpinephrine model.epinephrineTR "tracheaal"


printFluidBolus : Model -> String
printFluidBolus model =
    if model.fluidBolus == 0 then
        ""
    else
        (format (Locale 0 "," ".") model.fluidBolus) ++ " ml NaCl 0,9%"


printDefibrillation : Model -> String
printDefibrillation model =
    if model.defibrillation == 0 then
        ""
    else
        (format (Locale 0 "," ".") model.defibrillation) ++ " Joule"


printCardioversion : Model -> String
printCardioversion model =
    if model.cardioversion == 0 then
        ""
    else
        (format (Locale 0 "," ".") model.cardioversion) ++ " Joule"
