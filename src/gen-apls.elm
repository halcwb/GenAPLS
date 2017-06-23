module Main exposing (..)

import Html exposing (Html, text, div, button, input, Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)


-- Constants


no_age =
    -1


zero_age =
    0


half_age =
    0.5



-- Program


main =
    Html.beginnerProgram { model = model, view = view, update = update }



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



-- Update


type Msg
    = Reset
    | Update String
    | Calculate


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
                div50 x =
                    x / 50

                c =
                    ((a / 4)
                        + 4
                    )
                        * 50
                        |> ceiling
                        |> toFloat
                        |> div50
                        |> ceiling
                        |> toFloat

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
            10

        tr =
            100
    in
        { model
            | epinephrineIV = ( iv * model.weight, (iv / 1000) * model.weight, (iv / 10000) * model.weight )
            , epinephrineTR = ( tr * model.weight, (tr / 1000) * model.weight, (tr / 10000) * model.weight )
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            model

        Update txt ->
            let
                age =
                    case String.toFloat txt of
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
                    { model | age = age }
            in
                updated_model

        Calculate ->
            Debug.log "Calculate"
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



-- View


view : Model -> Html Msg
view model =
    let
        locale =
            Locale 1 "." ","

        printTubeSize model =
            let
                print n =
                    format locale n

                ( r, c, l ) =
                    model.tubeSize
            in
                (r |> print)
                    ++ " - "
                    ++ (c |> print)
                    ++ " - "
                    ++ (l |> print)

        printTubeLength n =
            format locale n ++ " cm"

        printTubeLengthOral model =
            printTubeLength model.tubeLengthOral

        printTubeLengthNasal model =
            printTubeLength model.tubeLengthNasal

        printEpinephrine e r =
            let
                ( d, s1, s2 ) =
                    e
            in
                (format locale d ++ " mcg" ++ " " ++ r ++ " = ")
                    ++ (format locale s1 ++ " ml van 0,1 mg/ml = 1:10.0000 oplossing of ")
                    ++ (format locale s2 ++ " ml van 1 mg/ml = 1:10000 oplossing")

        printEpinephrineIV model =
            printEpinephrine model.epinephrineIV "iv"

        printEpinephrineTR model =
            printEpinephrine model.epinephrineTR "tracheaal"

        printFluidBolus model =
            (format (Locale 0 "," ".") model.fluidBolus) ++ " ml NaCl 0,9%"

        printDefibrillation model =
            (format (Locale 0 "," ".") model.defibrillation) ++ " Joule"

        printCardioversion model =
            (format (Locale 0 "," ".") model.cardioversion) ++ " Joule"

        ageInput =
            let
                field =
                    [ name "age"
                    , type_ "number"
                    , Html.Attributes.max "18"
                    , onInput Update
                    , step "0.5"
                    ]
            in
                (if model.age == no_age then
                    [ placeholder "Leeftijd in jaren", value "" ]
                 else
                    [ value (toString model.age) ]
                )
                    |> List.append field
                    |> (\x -> input x [])
    in
        div [ style [ ( "margin", "50px" ) ] ]
            [ div [] [ text "Voer een leeftijd in" ]
            , ageInput
            , button [ onClick Calculate ] [ text "Bereken" ]
            , div [] [ text ("----------------------------------------") ]
            , div [] [ text ("Leeftijd: " ++ model.ageText) ]
            , div [] [ text ("Gewicht: " ++ (format locale model.weight) ++ " kg") ]
            , div [] [ text ("Tube maat: " ++ (model |> printTubeSize)) ]
            , div [] [ text ("Tube lengte oraal: " ++ (model |> printTubeLengthOral)) ]
            , div [] [ text ("Tube lengte nasaal: " ++ (model |> printTubeLengthNasal)) ]
            , div [] [ text ("Epinephrine iv: " ++ (model |> printEpinephrineIV)) ]
            , div [] [ text ("Epinephrine tracheaal: " ++ (model |> printEpinephrineTR)) ]
            , div [] [ text ("Vaat vulling: " ++ (model |> printFluidBolus)) ]
            , div [] [ text ("Defibrillatie: " ++ (model |> printDefibrillation)) ]
            , div [] [ text ("Cardioversie: " ++ (model |> printCardioversion)) ]
            ]
