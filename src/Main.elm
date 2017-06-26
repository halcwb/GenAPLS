module Main exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Html exposing (Attribute, Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (string)
import Model.Medication as D exposing (..)
import Model.Model as M exposing (..)
import Util.DomUtils exposing (..)
import Util.FloatUtils exposing (roundBy)
import VirtualDom


-- Program


main =
    Html.beginnerProgram { model = M.model, view = view, update = update }



-- Update


type Msg
    = Reset
    | UpdateAge String
    | UpdateWeight String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            M.model

        UpdateAge txt ->
            let
                meds =
                    Debug.log "Meds"
                        model.medications
                        |> List.map (\m -> createTr2 "" m D.print)
            in
            setAge txt model
                |> M.calculate

        UpdateWeight txt ->
            setWeight txt model
                |> M.calculate



-- View


stylesheetLink : String -> Html msg
stylesheetLink url =
    VirtualDom.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []


view : Model -> Html Msg
view model =
    let
        ageInput =
            let
                field =
                    [ name "age"
                    , type_ "number"
                    , Html.Attributes.max "18"
                    , Html.Attributes.min "-0.5"
                    , onInput UpdateAge
                    , step "0.5"
                    , width 30
                    , class "form-control"
                    , style [ ( "margin", "10px" ) ]
                    ]
            in
            Html.div [ class "form-group" ]
                [ Html.label [] [ text "Leeftijd (jaren)" ]
                , (if model.age == no_age then
                    [ placeholder "Leeftijd in jaren", value "" ]
                   else
                    [ value (toString model.age) ]
                  )
                    |> List.append field
                    |> (\xs -> input xs [])
                ]

        weightInput =
            let
                field =
                    [ name "weight"
                    , type_ "number"
                    , Html.Attributes.max "150"
                    , Html.Attributes.min "3"
                    , onInput UpdateWeight
                    , step "1"
                    , width 30
                    , class "form-control"
                    , style [ ( "margin", "10px" ) ]
                    ]
            in
            Html.div [ class "form-group" ]
                [ Html.label [] [ text "Gewicht (kg)" ]
                , (if model.weight == 0 then
                    [ placeholder "Gewicht in kg", value "" ]
                   else
                    [ value (toString model.weight) ]
                  )
                    |> List.append field
                    |> (\xs -> input xs [])
                ]
    in
    div [ style [ ( "margin", "50px" ) ] ]
        [ stylesheetLink "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        , p [ class "bg-primary", style [ ( "padding", "10px" ) ] ] [ text "Pediatrische Noodlijst Berekeningen" ]
        , Html.form [ class "form-inline", style [ ( "margin", "20px" ) ] ]
            [ ageInput
            , weightInput
            , button [ onClick Reset, class "btn btn-primary" ] [ text "Verwijderen" ]
            ]
        , Html.table [ class "table", class "table-hover", class "table-responsive" ]
            [ Html.caption [] [ text "Berekeningen" ]
            , Html.tbody []
                ([ createTr2 "tube maat" model printTubeSize
                 , createTr2 "tube lengte oraal" model printTubeLengthOral
                 , createTr2 "tube lengte nasaal" model printTubeLengthNasal
                 , createTr2 "epinephrine iv/io" model printEpinephrineIV
                 , createTr2 "epinephrine tracheaal" model printEpinephrineTR
                 , createTr2 "vaat vulling" model printFluidBolus
                 , createTr2 "defibrillatie" model printDefibrillation
                 , createTr2 "cardioversie" model printCardioversion
                 ]
                    ++ List.map (\m -> createTr2 m.name m D.printDoseVolume) model.medications
                )
            ]
        ]
