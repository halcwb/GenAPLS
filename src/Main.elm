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
import Util.ListUtils as List
import EmergencyList.EmergencyList as EmergencyList exposing (..)
import Component.CheckMenu as CheckMenu


-- Style


type Styles
    = Main
    | Header
    | Input
    | Label
    | Select
    | Button
    | TableHead
    | TableRow
    | TableRowHover


roboto : List Style.Font
roboto =
    [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } ]


stylesheet : Style.StyleSheet Styles variation
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
            , Font.size 14
            , Font.bold
            ]
        , Style.style Input
            [ Font.size 16
            , Color.text Color.teal900
            ]
        , Style.style Select
            [ Color.border Color.teal100
            , Border.bottom 1
            ]
        , Style.style Label
            [ Color.text Color.teal400
            , Font.size 14
            ]
        , Style.style TableHead
            [ Color.text <| Color.rgb 158 158 158
            , Font.size 14
            , Font.bold
            , Border.bottom 2
            , Color.border <| Color.lightGray
            ]
        , Style.style TableRow
            [ Color.text <| Color.rgb 33 33 33
            , Font.size 14
            , Border.bottom 1
            , Color.border <| Color.lightGray
            ]
        , Style.style TableRowHover
            [ Font.size 14
            , Color.text Color.black
            , Color.background Color.lightGray
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
            , hoverRowIndx = 0
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
    , hoverRowIndx : Int
    }


emptyModel : Model
emptyModel =
    let
        dropDown msg =
            Input.dropMenu Nothing msg
    in
        { emergencyList = EmergencyList.model
        , yearDropdown = dropDown UpdateYear
        , monthDropdown = dropDown UpdateMonth
        , hoverRowIndx = 0
        }



-- Update


type Msg
    = UrlChange Navigation.Location
    | UpdateYear (Input.SelectMsg String)
    | UpdateMonth (Input.SelectMsg String)
    | UpdateWeight String
    | Clear
    | TableRowEnter Int
    | TableRowLeave Int



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
                            model.emergencyList
                                |> EmergencyList.setAge age year_
                                |> EmergencyList.calculate

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

            UpdateWeight txt ->
                let
                    elist =
                        setWeight txt model.emergencyList |> EmergencyList.calculate
                in
                    ( { model | emergencyList = elist }
                    , Cmd.none
                    )

            Clear ->
                ( emptyModel, Cmd.none )

            TableRowEnter x ->
                ( { model | hoverRowIndx = x }, Cmd.none)

            TableRowLeave x ->
                ( { model | hoverRowIndx = 0 }, Cmd.none)



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
        numToString : Model -> a -> String
        numToString model n =
            if model.emergencyList.age < 0 then
                ""
            else
                toString n

        labelAbove s =
            Input.labelAbove <| Element.el Label [ Attributes.alignLeft ] (Element.text s)

        header =
            Element.header Header [ Attributes.padding 50 ] (Element.text "Pediatrische Noodlijst Berekeningen")

        ageDropdown txt min max dropDown =
            Input.select Select
                [ Attributes.padding 10 ]
                { label = labelAbove txt
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

        printList = model.emergencyList |> EmergencyList.printEmergencyList

        tableHead s =
            Element.el TableHead
                [ Attributes.alignLeft
                , Attributes.padding 10
                ]
                ( Element.text s)

        tableCell s i =
            let
                style =
                    if model.hoverRowIndx == i + 1 then TableRowHover else TableRow
            in
                Element.el
                    style
                    [ Attributes.alignLeft
                    , Attributes.padding 10
                    , Events.onMouseEnter (TableRowEnter (i + 1))
                    , Events.onMouseLeave (TableRowLeave (i + 1))
                    ]
                    ( Element.text s)
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
                    , Input.text Input
                        []
                        { onChange = UpdateWeight
                        , value = toString model.emergencyList.weight
                        , label = labelAbove "Gewicht (kg)"
                        , options = []
                        }
                    , Element.button Button [ Events.onClick Clear, Attributes.padding 15 ] (Element.text "VERWIJDER")
                    ]
                ,Element.el Main
                    [ Attributes.padding 50 ]
                        (Element.table Main
                            []
                            [ tableHead "Indicatie" ::  List.mapi (tableCell << .indication) printList
                            , tableHead "Interventie" ::  List.mapi (tableCell << .intervention) printList
                            , tableHead "" ::  List.mapi (tableCell << .value) printList
                            , tableHead "Bereiding" ::  List.mapi (tableCell << .preparation) printList
                            , tableHead "Advies" ::  List.mapi (tableCell << .advice) printList
                            ])
                ]
