module Main exposing (..)

-- HTML

import Html exposing (Attribute, Html, button, div, input, p, text, h1, h2, h3, h4)
import Html.Attributes exposing (..)
import Navigation


-- Material Desing

import Material
import Material.Color as Color
import Material.Scheme exposing (topWithScheme)
import Material.Button as Button
import Material.Options as Options
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Table as Table
import Material.Typography as Typography
import Material.Select as Select
import Material.Dropdown.Item as Item
import Material.Footer as Footer
import Material.Menu as Menu


-- Modules

import Util.DomUtils exposing (..)
import Util.Utils exposing (eqs)
import Util.FixPrecision exposing (fixPrecision)
import Model.Medication as Medication exposing (..)
import Model.EmergencyList as EmergencyList exposing (..)
import Component.CheckMenu as CheckMenu


-- Constants


selectCSS : String
selectCSS =
    "../vendor/elm-mdl/styles/select.css"



-- Program


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = EmergencyList.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Update


type Msg
    = Clear
    | UrlChange Navigation.Location
    | UpdateYear Int String
    | UpdateMonth Int String
    | UpdateWeight String
    | CheckWeight
    | Calculate
    | SelectIndicatie String
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( location |> EmergencyList.init |> Tuple.first, Cmd.none )

        Clear ->
            ( EmergencyList.model, Cmd.none )

        UpdateYear _ txt ->
            Debug.log ("UpdateYear: " ++ txt)
                ( setAge Year txt model |> EmergencyList.calculate
                , Cmd.none
                )

        UpdateMonth _ txt ->
            ( setAge Month txt model |> EmergencyList.calculate
            , Cmd.none
            )

        Calculate ->
            ( model |> EmergencyList.calculate
            , Cmd.none
            )

        UpdateWeight txt ->
            ( setWeight txt model |> EmergencyList.calculate
            , Cmd.none
            )

        CheckWeight ->
            ( if model.weight == 0 then
                { model | weightText = "" }
              else
                model
                    |> EmergencyList.calculate
            , Cmd.none
            )

        SelectIndicatie s ->
            let
                model_ =
                    EmergencyList.update s model
            in
                ( model_, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- View Components


indicatie : Model -> Html Msg
indicatie model =
    Menu.render Mdl
        []
        model.mdl
        [ Menu.bottomLeft ]
        (CheckMenu.view SelectIndicatie model.indicatieSelect)


clearBtn : Model -> Html Msg
clearBtn model =
    Button.render Mdl
        [ 9, 0, 0, 1 ]
        model.mdl
        [ Button.ripple
        , Button.colored
        , Button.raised
        , Options.onClick Clear
        ]
        [ text "Verwijder"
        ]


ageDropdown :
    Int
    -> Model
    -> String
    -> (number -> String -> Msg)
    -> c
    -> List a
    -> Html Msg
ageDropdown indx model lbl msg value range =
    Select.render Mdl
        [ indx ]
        model.mdl
        [ Select.label lbl
        , Select.floatingLabel
        , Select.below
        , Select.value
            (if model.age == EmergencyList.no_age then
                ""
             else
                toString value
            )
        , Options.attribute <| style [ ( "margin-right", "20px" ) ]
        ]
        (range
            |> List.map toString
            |> List.map
                (\s ->
                    Select.item
                        [ Item.onSelect (msg 0 s)
                        ]
                        [ text s
                        ]
                )
        )


yearDropdown : Model -> Html Msg
yearDropdown model =
    List.range 0 17
        |> ageDropdown 0 model "Leeftijd (jaren)" UpdateYear model.year


monthDropdown : Model -> Html Msg
monthDropdown model =
    List.range 0 11
        |> ageDropdown 1 model "Leeftijd (maanden)" UpdateMonth model.month


weightInput : Model -> Html Msg
weightInput model =
    Textfield.render Mdl
        [ 1 ]
        model.mdl
        [ Textfield.label "Gewicht (kg)"
        , Textfield.floatingLabel
        , Textfield.value model.weightText
        , Options.onInput UpdateWeight
        , Options.onBlur CheckWeight
        , Options.css "width" "150px"
        , Options.css "margin-right" "50px"
        ]
        []


emergencyList : Model -> Html Msg
emergencyList model =
    let
        createTd s =
            Table.td [ Options.cs "mdl-data-table__cell--non-numeric" ] [ text s ]

        theader =
            Table.thead []
                [ Table.th [] [ indicatie model ]
                , Table.th [] [ text "Indicatie" ]
                , createEl Table.th "Interventie" identity
                , createEl Table.th "" identity
                , createEl Table.th "Bereiding" identity
                , createEl Table.th "" identity
                , createEl Table.th "Advies" identity
                , createEl Table.th "" identity
                ]

        tbody =
            Table.tbody []
                (model
                    |> EmergencyList.emergencyList
                    |> List.map
                        (\m ->
                            [ ""
                            , m.indication
                            , m.intervention
                            , m.value
                            , m.preparation
                            , m.solution
                            , m.dose
                            , m.advice
                            ]
                                |> List.map createTd
                        )
                    |> List.map (Table.tr [])
                )
    in
        Table.table []
            [ theader
            , tbody
            ]



-- View


view : Model -> Html Msg
view model =
    let
        --
        -- Helper Functions
        --
        numToString : Model -> a -> String
        numToString model n =
            if model.age < 0 then
                ""
            else
                toString n

        header : Html msg
        header =
            h2 [ style [ ( "margin", "50px" ) ] ] [ text "Pediatrische Noodlijst Berekeningen" ]

        body =
            div [ style [ ( "margin", "50px" ) ] ]
                [ div []
                    [ yearDropdown model
                    , monthDropdown model
                    , weightInput model
                    , clearBtn model
                    ]
                , Options.div [ Typography.subhead ] [ "Berekeningen op basis van gewicht: " ++ (model.weight |> fixPrecision 2) ++ " kg" |> text ]
                , emergencyList model
                , Footer.mini [ Options.css "margin-top" "50px" ]
                    { left =
                        Footer.left []
                            [ Footer.logo [] [ Footer.html <| text "Informedica 2018" ]
                            ]
                    , right =
                        Footer.right []
                            [ Footer.logo [] [ Footer.html <| text "versie 0.2.0-beta" ]
                            ]
                    }
                ]
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { drawer = []
            , header = [ stylesheetLink selectCSS, header ]
            , main =
                [ div []
                    [ Material.Scheme.topWithScheme Color.Teal Color.Red body ]
                ]
            , tabs = ( [], [] )
            }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        ]
