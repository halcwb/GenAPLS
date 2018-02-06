module Main exposing (..)

-- import FormatNumber exposing (format)

import FormatNumber.Locales exposing (Locale)
import Html exposing (Attribute, Html, button, div, input, p, text, h1, h2, h3, h4)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (string)
import Model.Medication as D exposing (..)
import Model.Model as M exposing (..)
import Util.DomUtils exposing (..)
import Util.FloatUtils exposing (roundBy)
import VirtualDom
import Navigation
import Material
import Material.Color as Color
import Material.Scheme exposing (topWithScheme)
import Material.Button as Button
import Material.Options as Opts exposing (css, onClick)
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Table as Table


-- Program


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = M.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Update


type Msg
    = Clear
    | UrlChange Navigation.Location
    | UpdateAge String
    | UpdateWeight String
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( M.init location |> Tuple.first, Cmd.none )

        Clear ->
            ( M.model, Cmd.none )

        UpdateAge txt ->
            ( setAge txt model
                |> M.calculate
            , Cmd.none
            )

        UpdateWeight txt ->
            ( setWeight txt model
                |> M.calculate
            , Cmd.none
            )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- View


view : Model -> Html Msg
view model =
    let
        header =
            h2 [ style [ ( "padding", "10px" ) ] ] [ text "Pediatrische Noodlijst Berekeningen" ]

        clearBtn =
            Button.render Mdl
                [ 9, 0, 0, 1 ]
                model.mdl
                [ Button.ripple
                , Button.colored
                , Button.raised
                , Opts.onClick Clear
                ]
                [ text "Verwijderen" ]

        ageInput =
            Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "Leeftijd (jaren)"
                , Textfield.floatingLabel
                , Textfield.value (model.ageText)
                , Opts.onInput UpdateAge
                , Opts.css "width" "150px"
                , Opts.css "margin-right" "50px"
                ]
                []

        weightInput =
            Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Gewicht (kg)"
                , Textfield.floatingLabel
                , Textfield.value (toString model.weight)
                , Opts.onInput UpdateWeight
                , Opts.css "width" "150px"
                , Opts.css "margin-right" "50px"
                ]
                []

        body =
            let
                createTr =
                    createTr4 model

                emptyString =
                    (\_ -> "")

                printFst f m =
                    m |> f |> Tuple.first

                printSec f m =
                    let
                        s =
                            m |> f |> Tuple.second
                    in
                        if s == "" then
                            s
                        else
                            "= " ++ (m |> f |> Tuple.second)
            in
                div [ style [ ( "margin", "50px" ) ] ]
                    [ div []
                        [ ageInput
                        , weightInput
                        , clearBtn
                        ]
                    , p [] [ h3 [] [ text "Berekeningen" ] ]
                    , Table.table []
                        [ createTh4 "Categorie" "Item" "Waarde" "Oplossing"
                        , Table.tbody []
                            ([ createTr (\_ -> "reanimatie") (\_ -> "tube maat") printTubeSize emptyString
                             , createTr (\_ -> "reanimatie") (\_ -> "tube lengte oraal") printTubeLengthOral emptyString
                             , createTr (\_ -> "reanimatie") (\_ -> "tube lengte nasaal") printTubeLengthNasal emptyString
                             , createTr (\_ -> "reanimatie") (\_ -> "epinephrine iv/io") (printFst printEpinephrineIV) (printSec printEpinephrineIV)
                             , createTr (\_ -> "reanimatie") (\_ -> "epinephrine tracheaal") (printFst printEpinephrineTR) (printSec printEpinephrineTR)
                             , createTr (\_ -> "reanimatie") (\_ -> "vaat vulling") (printFst printFluidBolus) (printSec printFluidBolus)
                             , createTr (\_ -> "reanimatie") (\_ -> "defibrillatie") (printFst printDefibrillation) (printSec printDefibrillation)
                             , createTr (\_ -> "reanimatie") (\_ -> "cardioversie") (printFst printCardioversion) (printSec printCardioversion)
                             ]
                                ++ List.map (\m -> createTr4 m (\_ -> m.category) (\_ -> m.name) (printFst D.printDoseVolume) (printSec D.printDoseVolume)) model.medications
                            )
                        ]
                    ]
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { drawer = []
            , header = [ header ]
            , main =
                [ div []
                    [ Material.Scheme.topWithScheme Color.Teal Color.Red body ]
                ]
            , tabs = ( [], [] )
            }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
