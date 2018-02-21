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
import Style.Shadow as Shadow
import Json.Decode


-- Modules

import Util.DomUtils exposing (..)
import Util.FixPrecision exposing (fixPrecision)
import Util.ListUtils as List
import EmergencyList.EmergencyList as EmergencyList exposing (..)
import Component.CheckMenu as CheckMenu


-- Style


type Styles
    = NoStyle
    | Main
    | Header
    | Input
    | Label
    | Select
    | Button
    | TableHead
    | TableRow
    | TableRowHover
    | Title
    | MenuContents
    | MenuItem
    | MenuItemSelected
    | Footer


roboto : List Style.Font
roboto =
    [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } ]


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style Main
            [ Font.typeface roboto
            ]
        , Style.style Header
            [ Color.text Color.white
            , Color.background Color.teal800
            , Font.size 40
            ]
        , Style.style Button
            [ Border.rounded 5
            , Color.text Color.white
            , Color.background Color.teal900
            , Font.size 14
            , Font.bold
            , Style.hover [ Shadow.simple ]
            ]
        , Style.style Input
            [ Font.size 16
            , Color.text Color.teal900
            , Color.border Color.teal100
            , Border.bottom 1
            , Style.hover [ Shadow.simple ]
            , Style.focus [ Shadow.simple ]
            ]
        , Style.style Select
            [ Color.border Color.teal100
            , Border.bottom 1
            , Style.hover [ Shadow.simple ]
            ]
        , Style.style Label
            [ Color.text Color.teal400
            , Font.size 14
            ]
        , Style.style TableHead
            [ Color.text <| Color.rgb 158 158 158
            , Font.size 13
            , Font.bold
            , Border.bottom 2
            , Color.border <| Color.lightGray
            , Style.cursor "pointer"
            ]
        , Style.style TableRow
            [ Color.text <| Color.rgb 117 117 117

            --             Only hovers a cell not the whole row
            --             , Style.hover [ Color.background Color.lightGray
            --                           ]
            , Font.size 14
            , Border.bottom 1
            , Color.border <| Color.lightGray
            ]
        , Style.style TableRowHover
            [ Font.size 14
            , Color.text Color.black
            , Color.background Color.lightGray
            ]
        , Style.style Title
            [ Color.text <| Color.teal400
            , Font.size 16
            , Font.bold
            ]
        , Style.style MenuContents
            [ Color.background Color.white
            , Color.border Color.lightGray
            , Border.all 1
            , Shadow.simple
            ]
        , Style.style MenuItem
            [ Style.cursor "pointer"
            , Style.hover
                [ Color.background Color.lightGray
                ]
            , Color.text Color.black
            , Font.light
            ]
        , Style.style MenuItemSelected
            [ Style.cursor "pointer"
            , Color.background Color.lightGray
            , Color.text Color.black
            , Font.light
            ]
        , Style.style Footer
            [ Color.background <| Color.rgb 66 66 66
            , Color.text Color.white
            ]
        ]



-- Program


init :
    Navigation.Location
    -> ( Model, Cmd msg )
init location =
    let
        dropDown n msg =
            Input.dropMenu (Just <| toString n) msg

        elist =
            location |> EmergencyList.init

        model =
            { emergencyList = elist
            , yearDropdown = dropDown elist.year UpdateYear
            , monthDropdown = dropDown elist.month UpdateMonth
            , hoverRowIndx = 0
            , counter = 0
            , menuState = MenuClosed
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
    , counter : Int
    , menuState : MenuState
    }


type MenuState
    = MenuOpen
    | MenuClosed


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
        , counter = 0
        , menuState = MenuClosed
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
    | ToggleMenu
    | CloseMenu
    | SelectMenuItem String



{- This is a hack to ensure that
   the model and a text input field
   remains is sync. The model counter
   should be updated each time the input
   field changes through code, not by
   user input
-}


updateCounter : Model -> Model
updateCounter model =
    { model | counter = model.counter + 1 }


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
                    ( model_ |> updateCounter, Cmd.none )

            UpdateMonth selectMsg ->
                let
                    model_ =
                        let
                            ( emList, selMenu ) =
                                model.monthDropdown |> updateAge EmergencyList.Month selectMsg
                        in
                            { model | emergencyList = emList, monthDropdown = selMenu }
                in
                    ( model_ |> updateCounter, Cmd.none )

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
                ( { model | hoverRowIndx = x }, Cmd.none )

            TableRowLeave x ->
                ( { model | hoverRowIndx = 0 }, Cmd.none )

            ToggleMenu ->
                let
                    state =
                        case model.menuState of
                            MenuClosed ->
                                MenuOpen

                            MenuOpen ->
                                MenuClosed
                in
                    ( { model | menuState = state }, Cmd.none )

            CloseMenu ->
                ( { model | menuState = MenuClosed }, Cmd.none )

            SelectMenuItem s ->
                let
                    elist =
                        model.emergencyList |> EmergencyList.update s
                in
                    ( { model | emergencyList = elist }, Cmd.none )



-- View


onClickPreventDefault : msg -> Element.Attribute variation msg
onClickPreventDefault msg =
    Events.onWithOptions "click"
        { preventDefault = True, stopPropagation = True }
        (Json.Decode.succeed msg)


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
            Element.header Header [ Attributes.padding 30 ] (Element.text "Pediatrische Noodlijst Berekeningen")

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

        printList =
            model.emergencyList |> EmergencyList.printEmergencyList

        tableHead s =
            Element.el TableHead
                ([ Attributes.alignLeft
                 , Attributes.padding 10
                 ]
                    |> List.append
                        (if s |> String.startsWith "Indicatie" then
                            [ onClickPreventDefault ToggleMenu ]
                         else
                            []
                        )
                )
                (Element.text s)

        tableCell s i =
            let
                style =
                    if model.hoverRowIndx == i + 1 then
                        TableRowHover
                    else
                        TableRow
            in
                Element.el
                    style
                    [ Attributes.alignLeft
                    , Attributes.padding 10
                    , Events.onMouseEnter (TableRowEnter (i + 1))
                    , Events.onMouseLeave (TableRowLeave (i + 1))
                    ]
                    (Element.text s)

        tableTitle =
            if model.emergencyList.weight > 0 then
                "Berekend op basis van gewicht: "
                    ++ (fixPrecision 2 model.emergencyList.weight)
                    ++ " kg"
            else
                ""

        tableMenu s =
            let
                items =
                    model.emergencyList.indicatieSelect.all :: model.emergencyList.indicatieSelect.indications

                map item =
                    let
                        style =
                            if model.emergencyList.indicatieSelect.selections |> List.any ((==) item) then
                                MenuItemSelected
                            else
                                MenuItem
                    in
                        Element.el style [ Attributes.padding 15, onClickPreventDefault (SelectMenuItem item) ] <| Element.text item
            in
                case model.menuState of
                    MenuClosed ->
                        s
                            ++ " ▼"
                            |> tableHead

                    MenuOpen ->
                        s
                            ++ " ▲"
                            |> tableHead
                            |> Element.below
                                [ Element.column MenuContents
                                    [ Attributes.padding 10
                                    , Attributes.spacing 10
                                    , Events.onMouseLeave ToggleMenu
                                    ]
                                    (items
                                        |> List.map map
                                    )
                                ]
    in
        Element.viewport stylesheet <|
            Element.column Main
                [ Attributes.height Attributes.fill ]
                [ header
                , Element.row NoStyle
                    [ Attributes.padding 50
                    , Attributes.spacing 20
                    , Attributes.alignBottom
                    ]
                    [ yearDropdown
                    , monthDropdown
                    , Input.text Input
                        [ Attributes.padding 10 ]
                        { onChange = UpdateWeight
                        , value = model.emergencyList.weightText
                        , label = labelAbove "Gewicht (kg)"
                        , options = [ Input.textKey <| toString model.counter ]
                        }
                    , Element.button Button [ Events.onClick Clear, Attributes.padding 15 ] (Element.text "VERWIJDER")
                    ]
                , Element.paragraph Title
                    [ Attributes.paddingLeft 50 ]
                    [ Element.text tableTitle ]
                , Element.el NoStyle
                    [ Attributes.paddingLeft 50
                    , Attributes.paddingTop 20
                    , Attributes.paddingRight 50
                    , Attributes.paddingBottom 50
                    , Attributes.height Attributes.fill
                    , Attributes.yScrollbar
                    ]
                    (Element.table Main
                        []
                        [ ("Indicatie" |> tableMenu) :: List.mapi (tableCell << .indication) printList
                        , tableHead "Interventie" :: List.mapi (tableCell << .intervention) printList
                        , tableHead "Berekend" :: List.mapi (tableCell << .value) printList
                        , tableHead "Bereiding" :: List.mapi (tableCell << .preparation) printList
                        , tableHead "Advies" :: List.mapi (tableCell << .advice) printList
                        ]
                    )
                , Element.footer Footer
                    [ Attributes.paddingLeft 50
                    , Attributes.paddingTop 10
                    , Attributes.paddingBottom 10
                    ]
                    (Element.row NoStyle
                        [ Attributes.padding 15
                        , Attributes.spacing 20
                        ]
                        [ Element.link "http://github.com/halcwb/GenAPLS.git" <| Element.text "GenAPLS Informedica 2008"
                        , Element.link "https://www.eenheidintensievezorg.nl" <| Element.text "Eenheid Intensieve Zorg"
                        , Element.link "https://www.kinderformularium.nl" <| Element.text "Kinderformularium"
                        ]
                    )
                ]
