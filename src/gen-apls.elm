module Main exposing (..)

import Html exposing (Html, text, div, p, button, input, Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Util.Float exposing (roundBy)
import Util.Dom exposing (..)
import Model.Model exposing (..)


-- Program


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- Update


type Msg
    = Reset
    | Update String
    | Calculate


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            model

        Update txt ->
            setAge txt model

        Calculate ->
            Debug.log "Calculate"
                model
                |> calculate



-- View


view : Model -> Html Msg
view model =
    let
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
            , p [] [ text ("----------------------------------------") ]
            , div [] [ text ("Leeftijd: " ++ model.ageText) ]
            , div [] [ text ("Gewicht: " ++ (format locale1 model.weight) ++ " kg") ]
            , createP "Tube maat: " model printTubeSize
            , createP "Tube lengte oraal: " model printTubeLengthOral
            , createP "Tube lengte nasaal: " model printTubeLengthNasal
            , createP "Epinephrine iv: " model printEpinephrineIV
            , createP "Epinephrine tracheaal: " model printEpinephrineTR
            , createP "Vaat vulling: " model printFluidBolus
            , createP "Defibrillatie: " model printDefibrillation
            , createP "Cardioversie: " model printCardioversion
            ]
