module Main exposing (..)

-- import FormatNumber exposing (format)

import Html exposing (Attribute, Html, button, div, input, p, text, h1, h2, h3, h4)
import Html.Attributes exposing (..)
import Model.Medication as D exposing (..)
import Model.Model as M exposing (..)
import Util.DomUtils exposing (..)
import Navigation
import Material
import Material.Color as Color
import Material.Scheme exposing (topWithScheme)
import Material.Button as Button
import Material.Options as Options
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Table as Table
import Util.FixPrecision exposing (fixPrecision)
import Material.Typography as Typography
import Material.Select as Select
import Material.Dropdown.Item as Item
import VirtualDom
import Json.Encode as Encode
import Material.Footer as Footer
import Material.Menu as Menu
import Material.Icon as Icon


-- Constants


selectCSS : String
selectCSS =
    "../vendor/elm-mdl/styles/select.css"



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
            ( M.init location |> Tuple.first, Cmd.none )

        Clear ->
            ( M.model, Cmd.none )

        UpdateYear _ txt ->
            Debug.log ("UpdateYear: " ++ txt)
                ( setAge Year txt model |> M.calculate
                , Cmd.none
                )

        UpdateMonth _ txt ->
            ( setAge Month txt model |> M.calculate
            , Cmd.none
            )

        Calculate ->
            ( model |> M.calculate
            , Cmd.none
            )

        UpdateWeight txt ->
            ( setWeight txt model |> M.calculate
            , Cmd.none
            )

        CheckWeight ->
            ( if model.weight == 0 then
                { model | weightText = "" }
              else
                model
                    |> M.calculate
            , Cmd.none
            )

        SelectIndicatie ind ->
            ( if ind == "alles" then
                { model | indicatieSelect = [] }
              else if model.indicatieSelect |> List.any (\x -> x == ind) then
                { model | indicatieSelect = model.indicatieSelect |> List.filter (\x -> x /= ind) }
              else
                { model | indicatieSelect = model.indicatieSelect |> List.append [ ind ] }
            , Cmd.none
            )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- View


stylesheetLink : String -> Html msg
stylesheetLink url =
    VirtualDom.node
        "link"
        [ property "rel" (Encode.string "stylesheet")
        , property "type" (Encode.string "text/css")
        , property "href" (Encode.string url)
        ]
        []


view : Model -> Html Msg
view model =
    let
        eqs x1 x2 =
            x1 == x2

        checkmark x =
            if x then
                Icon.view "check" [ Options.css "width" "40px" ]
            else
                Options.span [ Options.css "width" "40px" ] []

        numToString n =
            if model.age < 0 then
                ""
            else
                toString n

        header =
            h2 [ style [ ( "margin", "50px" ) ] ] [ text "Pediatrische Noodlijst Berekeningen" ]

        createTr =
            createTr5 model emptyString

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

        indicatie =
            Menu.render Mdl
                [ 0 ]
                model.mdl
                [ Menu.bottomLeft ]
                [ Item.item
                    [ Item.onSelect (SelectIndicatie "alles") ]
                    [ checkmark (model.indicatieSelect |> List.isEmpty), text "Alles" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "reanimatie") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "reanimatie")), text "Reanimatie" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "intubatie") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "intubatie")), text "Intubatie" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "antidota") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "antidota")), text "Antidota" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "antiarrythmica") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "antiarrythmica")), text "Antiarrythmica" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "anticonvulsiva") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "anticonvulsiva")), text "Anticonvulsiva" ]
                , Item.item
                    [ Item.onSelect (SelectIndicatie "diversen") ]
                    [ checkmark (model.indicatieSelect |> List.any (eqs "diversen")), text "Diversen" ]
                ]

        createTh s1 s2 s3 s4 =
            Table.thead []
                [ Table.th [] [ indicatie ]
                , Table.th [] [ text s1 ]
                , createEl Table.th s2 identity
                , createEl Table.th s3 identity
                , createEl Table.th s4 identity
                ]

        clearBtn =
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

        yearDropdown =
            Select.render Mdl
                [ 0 ]
                model.mdl
                [ Select.label "Leeftijd (jaren)"
                , Select.floatingLabel
                , Select.below
                , Select.value
                    (if model.age == M.no_age then
                        ""
                     else
                        toString model.year
                    )
                , Options.attribute <| style [ ( "margin-right", "20px" ) ]
                ]
                (List.range 0 18
                    |> List.map toString
                    |> List.map
                        (\s ->
                            Select.item
                                [ Item.onSelect (UpdateYear 0 s)
                                ]
                                [ text s
                                ]
                        )
                )

        monthDropdown =
            Select.render Mdl
                [ 1 ]
                model.mdl
                [ Select.label "Leeftijd (maanden)"
                , Select.floatingLabel
                , Select.below
                , Select.value
                    (if model.age == M.no_age then
                        ""
                     else
                        toString model.month
                    )
                , Options.attribute <| style [ ( "margin-right", "20px" ) ]
                ]
                (List.range 0 11
                    |> List.map toString
                    |> List.map
                        (\s ->
                            Select.item
                                [ Item.onSelect (UpdateMonth 0 s)
                                ]
                                [ text s
                                ]
                        )
                )

        weightInput =
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

        table =
            ([ ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "tube maat") printTubeSize emptyString )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "tube lengte oraal") printTubeLengthOral emptyString )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "tube lengte nasaal") printTubeLengthNasal emptyString )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "epinephrine iv/io") (printFst printEpinephrineIV) (printSec printEpinephrineIV) )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "epinephrine tracheaal") (printFst printEpinephrineTR) (printSec printEpinephrineTR) )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "vaat vulling") (printFst printFluidBolus) (printSec printFluidBolus) )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "defibrillatie") (printFst printDefibrillation) (printSec printDefibrillation) )
             , ( "reanimatie", createTr (\_ -> "reanimatie") (\_ -> "cardioversie") (printFst printCardioversion) (printSec printCardioversion) )
             ]
                ++ List.map (\m -> ( m.category, createTr5 m emptyString (\_ -> m.category) (\_ -> m.name) (printFst D.printDoseVolume) (printSec D.printDoseVolume) )) model.medications
            )
                |> List.filter (\( ind, _ ) -> (model.indicatieSelect |> List.isEmpty) || (model.indicatieSelect |> List.any (eqs ind)))
                |> List.map Tuple.second

        body =
            div [ style [ ( "margin", "50px" ) ] ]
                [ div []
                    [ yearDropdown
                    , monthDropdown
                    , weightInput
                    , clearBtn
                    ]
                , Options.div [ Typography.subhead ] [ "Berekeningen op basis van gewicht: " ++ (model.weight |> fixPrecision 2) ++ " kg" |> text ]
                , Table.table []
                    [ createTh "Indicatie" "Interventie" "Waarde" "Bereiding"
                    , Table.tbody [] table
                    ]
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
