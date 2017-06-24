module Main exposing (..)

import VirtualDom
import Json.Encode exposing (string)
import Html exposing (Html, text, div, p, button, input, Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Util.Float exposing (roundBy)
import Util.Dom exposing (..)
import Model.Model as M exposing (..)


-- Program


main =
    Html.beginnerProgram { model = M.model, view = view, update = update }



-- Update


type Msg
    = Reset
    | Update String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            M.model

        Update txt ->
            (setAge txt model)
                |> calculate



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
                    , onInput Update
                    , step "0.5"
                    , width 30
                    , class "form-control"
                    , style [ ( "margin", "10px" ) ]
                    ]
            in
                (if model.age == no_age then
                    [ placeholder "Leeftijd in jaren", value "" ]
                 else
                    [ value (toString model.age) ]
                )
                    |> List.append field
                    |> (\xs -> input xs [])
    in
        div [ style [ ( "margin", "50px" ) ] ]
            [ stylesheetLink "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            , p [ class "bg-primary", style [ ( "padding", "10px" ) ] ] [ text "Voer een leeftijd in" ]
            , Html.form [ class "form-inline" ]
                [ ageInput
                , button [ onClick Reset, class "btn btn-primary" ] [ text "Verwijderen" ]
                ]
            , Html.table [ class "table" ]
                [ Html.caption [] [ text ("Berekeningen") ]
                , Html.tbody
                    []
                    [ createTr2 "Leeftijd" model printAge
                    , createTr2 "Gewicht" model printWeight
                    , createTr2 "Tube maat" model printTubeSize
                    , createTr2 "Tube lengte oraal" model printTubeLengthOral
                    , createTr2 "Tube lengte nasaal" model printTubeLengthNasal
                    , createTr2 "Epinephrine iv/io" model printEpinephrineIV
                    , createTr2 "Epinephrine tracheaal" model printEpinephrineTR
                    , createTr2 "Vaat vulling" model printFluidBolus
                    , createTr2 "Defibrillatie" model printDefibrillation
                    , createTr2 "Cardioversie" model printCardioversion
                    ]
                ]
            ]
