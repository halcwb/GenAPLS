module Main exposing (..)

-- Style

import GenStyle as Style


-- HTML

import Html exposing (Html)
import Navigation
import Element as Element
import Element.Attributes as Attributes
import Element.Events as Events
import Element.Input as Input
import Json.Decode
import Window


-- Modules

import Util.FixPrecision exposing (fixPrecision)
import Util.ListUtils as List
import Page.EmergencyList as EmergencyList exposing (..)


-- Program


type alias Device =
    { width : Int
    , height : Int
    , userAgent : String
    , supportsGrid : Bool
    }


init : Device -> Navigation.Location -> ( Model, Cmd msg )
init device location =
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
            , device = device
            }
    in
        ( model, Cmd.none )


main : Program Device Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { emergencyList : EmergencyList.Model
    , yearDropdown : Input.SelectWith String Msg
    , monthDropdown : Input.SelectWith String Msg
    , hoverRowIndx : Int
    , counter : Int
    , menuState : MenuState
    , device : Device
    }


type MenuState
    = MenuOpen
    | MenuClosed


emptyModel : Model -> Model
emptyModel model =
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
        , device = model.device
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
    | Resize Window.Size



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
            -- Navigation
            --
            UrlChange location ->
                let
                    elist =
                        location |> EmergencyList.init
                in
                    ( { model | emergencyList = elist }, Cmd.none )

            -- Handle patient data
            --
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
                ( emptyModel model, Cmd.none )

            -- Handle table events
            --
            TableRowEnter x ->
                ( { model | hoverRowIndx = x }, Cmd.none )

            TableRowLeave x ->
                ( { model | hoverRowIndx = 0 }, Cmd.none )

            -- Handle menu events
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

            Resize size ->
                let
                    device =
                        model.device

                    { height, width } =
                        size
                in
                    ( { model | device = { device | width = width, height = height } }, Cmd.none )



-- View


onClickPreventDefault : msg -> Element.Attribute variation msg
onClickPreventDefault msg =
    Events.onWithOptions "click"
        { preventDefault = True, stopPropagation = True }
        (Json.Decode.succeed msg)



-- Navbar


navBar : Model -> Element.Element Style.Styles variation msg
navBar model =
    let
        title =
            "Pediatrische Noodlijst Berekeningen"
    in
        Element.header Style.Header [ Attributes.padding 30 ] (Element.text title)



-- Body


body : Model -> Element.Element Style.Styles variation Msg
body model =
    let
        tableTitle =
            if model.emergencyList.weight > 0 then
                "Berekend op basis van gewicht: "
                    ++ (fixPrecision 2 model.emergencyList.weight)
                    ++ " kg"
            else
                "Berekend op basis van gewicht: "

        tableHead s =
            Element.el Style.TableHead
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

        ageDropdown txt min max dropDown =
            Input.select Style.Select
                [ Attributes.padding 10 ]
                { label = labelAbove txt
                , with = dropDown
                , max = max + 1
                , options = []
                , menu =
                    Input.menu Style.Select
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

        labelAbove s =
            Input.labelAbove <| Element.el Style.Label [ Attributes.alignLeft ] (Element.text s)

        tableCell s i =
            let
                style =
                    if model.hoverRowIndx == i + 1 then
                        Style.TableRowHover
                    else
                        Style.TableRow
            in
                Element.el
                    style
                    [ Attributes.alignLeft
                    , Attributes.padding 10
                    , Events.onMouseEnter (TableRowEnter (i + 1))
                    , Events.onMouseLeave (TableRowLeave (i + 1))
                    ]
                    (Element.text s)

        tableMenu s =
            let
                items =
                    model.emergencyList.indicatieSelect.all :: model.emergencyList.indicatieSelect.indications

                map item =
                    let
                        style =
                            if model.emergencyList.indicatieSelect.selections |> List.any ((==) item) then
                                Style.MenuItemSelected
                            else
                                Style.MenuItem
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
                                [ Element.column Style.MenuContents
                                    [ Attributes.padding 10
                                    , Attributes.spacing 10
                                    , Events.onMouseLeave ToggleMenu
                                    ]
                                    (items
                                        |> List.map map
                                    )
                                ]
    in
        Element.column Style.Main
            [ Attributes.height Attributes.fill ]
            [ Element.row Style.None
                [ Attributes.paddingLeft 50
                , Attributes.paddingTop 20
                , Attributes.paddingBottom 20
                , Attributes.paddingRight 50
                , Attributes.alignRight
                , Attributes.spacing 50
                , Attributes.alignBottom
                ]
                [ yearDropdown
                , monthDropdown
                , Input.text Style.Input
                    [ Attributes.padding 10 ]
                    { onChange = UpdateWeight
                    , value = model.emergencyList.weightText
                    , label = labelAbove "Gewicht (kg)"
                    , options = [ Input.textKey <| toString model.counter ]
                    }
                , Element.button Style.Button [ Events.onClick Clear, Attributes.padding 10 ] (Element.text "VERWIJDER")
                ]
            , Element.paragraph Style.Title
                [ Attributes.paddingLeft 50
                , Attributes.paddingBottom 10
                ]
                [ Element.text tableTitle ]
            , Element.el Style.None
                [ Attributes.paddingLeft 50
                , Attributes.paddingRight 50
                , Attributes.paddingBottom 50
                , Attributes.height Attributes.fill
                , Attributes.yScrollbar
                ]
                (Element.table Style.Main
                    []
                    [ ("Indicatie" |> tableMenu) :: List.mapi (tableCell << .indication) printList
                    , tableHead "Interventie" :: List.mapi (tableCell << .intervention) printList
                    , tableHead "Berekend" :: List.mapi (tableCell << .value) printList
                    , tableHead "Bereiding" :: List.mapi (tableCell << .preparation) printList
                    , tableHead "Advies" :: List.mapi (tableCell << .advice) printList
                    ]
                )
            ]



-- Footer


footer : Element.Element Style.Styles variation msg
footer =
    Element.footer Style.Footer
        [ Attributes.paddingLeft 50
        , Attributes.paddingTop 10
        , Attributes.paddingBottom 10
        ]
        (Element.row Style.None
            [ Attributes.padding 15
            , Attributes.spacing 20
            ]
            [ Element.link "http://github.com/halcwb/GenAPLS.git" <| Element.text "GenAPLS Informedica 2008"
            , Element.link "https://www.eenheidintensievezorg.nl" <| Element.text "Eenheid Intensieve Zorg"
            , Element.link "https://www.kinderformularium.nl" <| Element.text "Kinderformularium"
            ]
        )



-- View


view : Model -> Html Msg
view model =
    if model.device.supportsGrid then
        Element.viewport Style.stylesheet <|
            Element.column Style.Main
                [ Attributes.height Attributes.fill ]
                [ navBar model
                , body model
                , footer
                ]
    else
        Element.viewport Style.stylesheet <|
            Element.column Style.Main
                [ Attributes.height Attributes.fill ]
                [ navBar model
                , Element.el Style.None [ Attributes.height Attributes.fill ] <| Element.text "Oops kan met deze browser de inhoud niet weergeven"
                , footer
                ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize
