module Page.EmergencyList exposing (..)

import Navigation
import Element as Element
import Element.Attributes as Attributes
import Element.Events as Events
import Element.Input as Input
import GenStyle as Style
import Data.Intervention as Intervention
import Data.Medication as Medication
import Util.FixPrecision as US exposing (fixPrecision)
import Util.ListUtils as List
import Json.Decode


-- Helper


onClickPreventDefault : msg -> Element.Attribute variation msg
onClickPreventDefault msg =
    Events.onWithOptions "click"
        { preventDefault = True, stopPropagation = True }
        (Json.Decode.succeed msg)



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



-- Model


type Age
    = Year
    | Month


type MenuState
    = MenuOpen
    | MenuClosed


type alias Model =
    { interventions : Intervention.Model
    , indications : List String
    , selections : List String
    , all : String
    , yearDropdown : Input.SelectWith String Msg
    , monthDropdown : Input.SelectWith String Msg
    , hoverRowIndx : Int
    , menuState : MenuState
    , counter : Int
    }


newModel : Model
newModel =
    let
        inds =
            Medication.medicationList
                |> List.map .category
                |> List.removeDuplicates

        dropDown msg =
            Input.dropMenu Nothing msg
    in
        { interventions = Intervention.model
        , indications = inds
        , selections = []
        , all = "alles"
        , yearDropdown = dropDown UpdateYear
        , monthDropdown = dropDown UpdateMonth
        , hoverRowIndx = 0
        , menuState = MenuClosed
        , counter = 0
        }


init : Navigation.Location -> Model
init location =
    { newModel | interventions = Intervention.init location.search }



-- Print


type alias Intervention =
    { indication : String
    , intervention : String
    , value : String
    , preparation : String
    , solution : String
    , advice : String
    }


printEmergencyList : Model -> List Intervention
printEmergencyList model =
    let
        printTubeSize =
            Intervention.printTubeSize

        printTubeLengthOral =
            Intervention.printTubeLengthOral

        printTubeLengthNasal =
            Intervention.printTubeLengthNasal

        printEpinephrineIVFst =
            Intervention.printEpinephrineIV
                >> Tuple.first

        printEpinephrineIVSnd =
            Intervention.printEpinephrineIV
                >> Tuple.second

        printEpinephrineTrFst =
            Intervention.printEpinephrineTR
                >> Tuple.first

        printEpinephrineTrSnd =
            Intervention.printEpinephrineTR
                >> Tuple.second

        printFluidBolusFst =
            Intervention.printFluidBolus
                >> Tuple.first

        printFluidBolusSnd =
            Intervention.printFluidBolus
                >> Tuple.second

        printDefibrillationFst =
            Intervention.printDefibrillation
                >> Tuple.first

        printDefibrillationSnd =
            Intervention.printDefibrillation
                >> Tuple.second

        printCardioversionFst =
            Intervention.printCardioversion
                >> Tuple.first

        printCardioversionSnd =
            Intervention.printCardioversion
                >> Tuple.second

        intervs =
            model.interventions

        meds =
            let
                dosePerKg m =
                    m.dose
                        / intervs.weight
                        |> fixPrecision 1

                dose m =
                    ((m |> Medication.printDoseVolume >> Tuple.first)
                        ++ " ("
                        ++ dosePerKg m
                        ++ " "
                        ++ m.unit
                        ++ "/kg)"
                    )

                volume m =
                    m
                        |> Medication.printDoseVolume
                        |> Tuple.second

                advice m =
                    m
                        |> Medication.printAdvice
            in
                List.map (\m -> Intervention m.category m.name (m |> dose) (m |> volume) "" (m |> advice)) intervs.medications
    in
        ([ Intervention "reanimatie" "tube maat" (printTubeSize intervs) "" "" "4 + Leeftijd / 4"
         , Intervention "reanimatie" "tube lengte oraal" (printTubeLengthOral intervs) "" "" "12 + Leeftijd / 2"
         , Intervention "reanimatie" "tube lengte nasaal" (printTubeLengthNasal intervs) "" "" "15 + Leeftijd / 2"
         , Intervention "reanimatie" "epinephrine iv/io" (intervs |> printEpinephrineIVFst) (intervs |> printEpinephrineIVSnd) "" "0,01 mg/kg iv"
         , Intervention "reanimatie" "epinephrine tracheaal" (intervs |> printEpinephrineTrFst) (intervs |> printEpinephrineTrSnd) "" "0,1 mg/kg trach"
         , Intervention "reanimatie" "vaat vulling" (intervs |> printFluidBolusFst) (intervs |> printFluidBolusSnd) "" "20 ml/kg"
         , Intervention "reanimatie" "defibrillatie" (intervs |> printDefibrillationFst) (intervs |> printDefibrillationSnd) "" "4 Joule/kg"
         , Intervention "reanimatie" "cardioversie" (intervs |> printCardioversionFst) (intervs |> printCardioversionSnd) "" "2 Joule/kg"
         ]
            ++ meds
        )
            |> List.filter (\m -> (model.selections |> List.isEmpty) || (model.selections |> List.any ((==) m.indication)))



-- Update


type Msg
    = UpdateYear (Input.SelectMsg String)
    | UpdateMonth (Input.SelectMsg String)
    | UpdateWeight String
    | Clear
    | TableRowEnter Int
    | TableRowLeave
    | ToggleMenu
    | CloseMenu
    | SelectMenuItem String


setAge : Age -> String -> Model -> Model
setAge age string model =
    let
        age_ =
            case age of
                Year ->
                    Intervention.Year

                Month ->
                    Intervention.Month
    in
        { model | interventions = model.interventions |> Intervention.setAge age_ string }


setWeight : String -> Model -> Model
setWeight string model =
    { model | interventions = model.interventions |> Intervention.setWeight string }


calculate : Model -> Model
calculate model =
    { model | interventions = model.interventions |> Intervention.calculate }


updateModel : String -> Model -> Model
updateModel s model =
    let
        model_ =
            if s == model.all then
                { model | selections = [] }
            else if model.selections |> List.any ((==) s) then
                { model | selections = model.selections |> List.filter (\x -> not (x == s)) }
            else
                { model | selections = model.selections |> List.append [ s ] }
    in
        model_


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAge age selectMsg menu =
            let
                selMenu =
                    menu |> Input.updateSelection selectMsg

                selected =
                    selMenu |> Input.selected

                model_ =
                    case selected of
                        Just a ->
                            model
                                |> setAge age a
                                |> calculate

                        Nothing ->
                            model
            in
                ( model_, selMenu )
    in
        case msg of
            -- Handle patient data
            --
            UpdateYear selectMsg ->
                let
                    model_ =
                        let
                            ( newModel, selMenu ) =
                                model.yearDropdown |> updateAge Year selectMsg
                        in
                            { newModel | yearDropdown = selMenu }
                in
                    ( model_ |> updateCounter, Cmd.none )

            UpdateMonth selectMsg ->
                let
                    model_ =
                        let
                            ( newModel, selMenu ) =
                                model.monthDropdown |> updateAge Month selectMsg
                        in
                            { newModel | monthDropdown = selMenu }
                in
                    ( model_ |> updateCounter, Cmd.none )

            UpdateWeight txt ->
                let
                    model_ =
                        setWeight txt model |> calculate
                in
                    ( model_
                    , Cmd.none
                    )

            Clear ->
                ( newModel, Cmd.none )

            -- Handle table events
            --
            TableRowEnter x ->
                ( { model | hoverRowIndx = x }, Cmd.none )

            TableRowLeave ->
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
                    model_ =
                        model |> updateModel s
                in
                    ( { model_ | menuState = MenuClosed }, Cmd.none )



-- View


body : Bool -> Model -> Element.Device -> Element.Element Style.Styles variation Msg
body supportsGrid model device =
    let
        tableTitle =
            if model.interventions.weight > 0 then
                "Berekend op basis van gewicht: "
                    ++ (fixPrecision 2 model.interventions.weight)
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
                            [ onClickPreventDefault ToggleMenu
                            , Attributes.minWidth <| Attributes.px 200
                            ]
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
                    Input.menu Style.MenuContents
                        []
                        (List.range min max
                            |> List.map toString
                            |> List.map
                                (\x ->
                                    Input.choice x <|
                                        Element.el Style.MenuItem
                                            []
                                        <|
                                            Element.text x
                                )
                        )
                }

        yearDropdown =
            model.yearDropdown |> ageDropdown "Leeftijd (jaren)" 0 17

        monthDropdown =
            model.monthDropdown |> ageDropdown "Leeftijd (maanden)" 0 11

        printList =
            model |> printEmergencyList

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
                    , Events.onMouseLeave TableRowLeave
                    ]
                    (Element.text s)

        oneColumn r i =
            let
                style =
                    if model.hoverRowIndx == i + 1 then
                        Style.TableRowHover
                    else
                        Style.TableRow
            in
                Element.column
                    style
                    [ Attributes.alignLeft
                    , Attributes.padding 10
                    , Events.onMouseEnter (TableRowEnter (i + 1))
                    , Events.onMouseLeave TableRowLeave
                    ]
                    [ Element.underline r.indication
                    , Element.bold <|
                        r.intervention
                            ++ " "
                            ++ r.value
                    , if r.preparation == "" then
                        Element.empty
                      else
                        Element.paragraph Style.None
                            [ Attributes.maxWidth <| Attributes.px 300 ]
                            [ Element.text <| "bereiding: " ++ r.preparation ]
                    , if r.solution == "" then
                        Element.empty
                      else
                        Element.text r.solution
                    , Element.italic <| "advies: " ++ r.advice
                    ]

        tableMenu s =
            let
                items =
                    model.all :: model.indications

                map item =
                    let
                        style =
                            if List.isEmpty model.selections && item == model.all then
                                Style.MenuItemSelected
                            else if model.selections |> List.any ((==) item) then
                                Style.MenuItemSelected
                            else
                                Style.MenuItem
                    in
                        Element.el style [ Attributes.padding 10, onClickPreventDefault (SelectMenuItem item) ] <| Element.text item
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

        table =
            let
                columns =
                    [ tableMenu "Indicatie" :: List.mapi (tableCell << .indication) printList
                    , tableHead "Interventie" :: List.mapi (tableCell << .intervention) printList
                    , tableHead "Berekend" :: List.mapi (tableCell << .value) printList
                    , tableHead "Bereiding" :: List.mapi (tableCell << .preparation) printList
                    , tableHead "Advies" :: List.mapi (tableCell << .advice) printList
                    ]
            in
                if device.phone || not supportsGrid then
                    Element.column Style.None [] <| tableHead "Berekend" :: List.mapi oneColumn printList
                else
                    Element.table Style.Main
                        []
                        columns

        input =
            if device.phone || not supportsGrid then
                Element.column Style.None
                    [ Attributes.paddingTop 20
                    , Attributes.paddingBottom 20
                    , Attributes.spacing 20
                    , Attributes.alignBottom
                    ]
                    [ Element.row Style.None
                        []
                        [ yearDropdown
                        , monthDropdown
                        ]
                    , Element.row Style.None
                        [ Attributes.spacing 20 ]
                        [ Input.text Style.Input
                            [ Attributes.maxWidth <| Attributes.px 200 ]
                            { onChange = UpdateWeight
                            , value = model.interventions.weightText
                            , label = labelAbove "Gewicht (kg)"
                            , options = [ Input.textKey <| toString model.counter ]
                            }
                        , tableMenu "Indicatie"
                        ]
                    , Element.button Style.Button
                        [ Events.onClick Clear
                        , Attributes.padding 10
                        ]
                        (Element.text "VERWIJDER")
                    ]
            else
                Element.row Style.None
                    [ Attributes.paddingTop 20
                    , Attributes.paddingBottom 20
                    , Attributes.spacing 50
                    , Attributes.alignRight
                    , Attributes.alignBottom
                    ]
                    [ yearDropdown
                    , monthDropdown
                    , Input.text Style.Input
                        []
                        { onChange = UpdateWeight
                        , value = model.interventions.weightText
                        , label = labelAbove "Gewicht (kg)"
                        , options = [ Input.textKey <| toString model.counter ]
                        }
                    , Element.button Style.Button
                        [ Events.onClick Clear
                        , Attributes.padding 10
                        ]
                        (Element.text "VERWIJDER")
                    ]
    in
        Element.column Style.Main
            [ Attributes.height Attributes.fill
            , Attributes.width Attributes.fill
            , if device.phone || not supportsGrid then
                Attributes.yScrollbar
              else
                Attributes.height Attributes.fill
            ]
            [ input
            , Element.paragraph Style.Title
                (addPaddingTopBottom10 [])
                [ Element.text tableTitle ]
            , Element.column Style.None
                [ Attributes.paddingBottom 50
                , Attributes.height Attributes.fill
                , Attributes.width Attributes.fill
                , if device.phone || not supportsGrid then
                    Attributes.paddingBottom 50
                  else
                    Attributes.yScrollbar
                ]
                [ table ]
            ]



-- Helper functions


addPaddingTopBottom10 :
    List (Element.Attribute variation msg)
    -> List (Element.Attribute variation msg)
addPaddingTopBottom10 attrs =
    attrs
        |> List.append
            [ Attributes.paddingTop 10
            , Attributes.paddingBottom 10
            ]
