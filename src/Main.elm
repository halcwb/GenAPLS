module Main exposing (..)

-- HTML

import Html exposing (Html)
import Navigation
import MaterialColor as Color
import Color
import Element as Element
import Element.Attributes as Attributes
import Element.Events as Events
import Element.Input as Input
import Style as Style
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border


-- Modules

import Util.DomUtils exposing (..)
import Util.FixPrecision exposing (fixPrecision)
import EmergencyList.EmergencyList as EmergencyList exposing (..)
import Component.CheckMenu as CheckMenu


-- Style


type Styles
    = Main
    | Header
    | Input
    | Select
    | Button


roboto =
    [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } ]


stylesheet =
    Style.styleSheet
        [ Style.style Main
            [ Font.typeface roboto
            ]
        , Style.style Header
            [ Color.text Color.white
            , Color.background Color.teal500
            , Font.size 45
            ]
        , Style.style Button
            [ Border.rounded 5
            , Color.text Color.white
            , Color.background Color.teal500
            , Font.size 16
            ]
        , Style.style Input
            [ Font.size 16
            , Color.text Color.teal900
            ]
        , Style.style Select
            [ Color.border Color.teal100
            , Border.bottom 1
            ]
        ]



-- Program


init :
    Navigation.Location
    -> ( Model, Cmd msg )
init location =
    let
        dropDown msg =
            Input.dropMenu Nothing msg

        model =
            { emergencyList = location |> EmergencyList.init
            , yearDropdown = dropDown UpdateYear
            , monthDropdown = dropDown UpdateMonth
            }
    in
        ( model, Cmd.none )


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }



-- Model


type alias Model =
    { emergencyList : EmergencyList.Model
    , yearDropdown : Input.SelectWith String Msg
    , monthDropdown : Input.SelectWith String Msg
    }


emptyModel =
    let
        dropDown msg =
            Input.dropMenu Nothing msg
    in
        { emergencyList = EmergencyList.model
        , yearDropdown = dropDown UpdateYear
        , monthDropdown = dropDown UpdateMonth
        }



-- Update


type
    Msg
    {- = Clear
       | UrlChange Navigation.Location
    -}
    = UrlChange Navigation.Location
    | UpdateYear (Input.SelectMsg String)
    | UpdateMonth (Input.SelectMsg String)
    | Clear



--     | UpdateWeight String
--     | CheckWeight
--     | Calculate
--     | SelectIndicatie String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAge age selectMsg menu =
            let
                selMenu =
                    menu |> Input.updateSelection selectMsg

                year =
                    selMenu |> Input.selected

                emList =
                    case year of
                        Just year_ ->
                            model.emergencyList |> EmergencyList.setAge age year_

                        Nothing ->
                            model.emergencyList
            in
                ( emList, selMenu )
    in
        case msg of
            UrlChange location ->
                let
                    elist =
                        location |> EmergencyList.init
                in
                    ( { model | emergencyList = elist }, Cmd.none )

            UpdateYear selectMsg ->
                let
                    model_ =
                        let
                            ( emList, selMenu ) =
                                model.yearDropdown |> updateAge EmergencyList.Year selectMsg
                        in
                            { model | emergencyList = emList, yearDropdown = selMenu }
                in
                    ( model_, Cmd.none )

            UpdateMonth selectMsg ->
                let
                    model_ =
                        let
                            ( emList, selMenu ) =
                                model.monthDropdown |> updateAge EmergencyList.Month selectMsg
                        in
                            { model | emergencyList = emList, monthDropdown = selMenu }
                in
                    ( model_, Cmd.none )

            Clear ->
                ( emptyModel, Cmd.none )



--         Calculate ->
--             ( model |> EmergencyList.calculate
--             , Cmd.none
--             )
--         UpdateWeight txt ->
--             ( setWeight txt model |> EmergencyList.calculate
--             , Cmd.none
--             )
--         CheckWeight ->
--             ( if model.weight == 0 then
--                 { model | weightText = "" }
--               else
--                 model
--                     |> EmergencyList.calculate
--             , Cmd.none
--             )
--         SelectIndicatie s ->
--             let
--                 model_ =
--                     EmergencyList.update s model
--             in
--                 ( model_, Cmd.none )
-- View Components
-- View


view : Model -> Html Msg
view model =
    let
        --
        -- Helper Functions
        --
        numToString : Model -> a -> String
        numToString model n =
            if model.emergencyList.age < 0 then
                ""
            else
                toString n

        header =
            Element.header Header [ Attributes.padding 50 ] (Element.text "Pediatrische Noodlijst Berekeningen")

        ageDropdown txt min max dropDown =
            Input.select Select
                [ Attributes.padding 10 ]
                { label = Input.labelAbove <| Element.text txt
                , with = dropDown
                , max = max + 1
                , options = []
                , menu =
                    Input.menu Select
                        []
                        (List.range min max
                            |> List.map toString
                            |> List.map (\x -> Input.choice x <| Element.text x)
                        )
                }

        yearDropdown =
            model.yearDropdown |> ageDropdown "Leeftijd (jaren)" 0 17

        monthDropdown =
            model.monthDropdown |> ageDropdown "Leeftijd (maanden)" 0 11
    in
        Element.viewport stylesheet <|
            Element.column Main
                [ Attributes.height Attributes.fill ]
                [ header
                , Element.row Input
                    [ Attributes.padding 50
                    , Attributes.spacing 20
                    ]
                    [ yearDropdown
                    , monthDropdown
                    , Element.button Button [ Events.onClick Clear, Attributes.padding 10 ] (Element.text "Verwijderen")
                    ]
                ]
