module Main exposing (..)

-- Style

import GenStyle as Style


-- HTML

import Html exposing (Html)
import Navigation
import Element as Element
import Element.Attributes as Attributes
import Window


-- Modules

import Page.EmergencyList as EmergencyList exposing (..)


-- Model


type alias Model =
    { emergencyList : EmergencyList.Model
    , device : Device
    }



-- Program


type alias Device =
    { size : Window.Size
    , userAgent : String
    , supportsGrid : Bool
    }


init : Device -> Navigation.Location -> ( Model, Cmd msg )
init device location =
    let

        elist =
            location |> EmergencyList.init

        model =
            { emergencyList = elist
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



-- Update


type Msg
    = UrlChange Navigation.Location
    | EListMsg EmergencyList.Msg
    | Resize Window.Size




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of
            -- Navigation
            --
            UrlChange location ->
                let
                    elist =
                        location |> EmergencyList.init
                in
                    ( { model | emergencyList = elist }, Cmd.none )

            EListMsg msg ->
                ( { model | emergencyList = model.emergencyList |> EmergencyList.update msg |> Tuple.first }
                , Cmd.none
                )


            Resize size ->
                let
                    device =
                        model.device


                in
                    ( { model | device = { device | size = size } }, Cmd.none )



-- View


-- Navbar


navBar : Model -> Element.Element Style.Styles variation msg
navBar model =
    let
        title =
            "Pediatrische Noodlijst Berekeningen"
    in
        Element.header Style.Header [ Attributes.padding 30 ] (Element.text title)



-- Footer


footer : Model -> Element.Element Style.Styles variation msg
footer model =
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
            , Element.text <| "Browser size: " ++ (toString model.device.size.width) ++ " x " ++ (toString model.device.size.height)
            ]
        )



-- Body

body : Model -> Element.Element Style.Styles variation Msg
body model =
    Element.column
        Style.None
        [ Attributes.center
        , Attributes.height Attributes.fill
        , Attributes.paddingLeft 20
        , Attributes.paddingRight 20
        ]
        [ model.device.size
              |> Element.classifyDevice
              |> EmergencyList.body model.emergencyList
              |> Element.map EListMsg
        ]


-- View



view : Model -> Html Msg
view model =
    if model.device.supportsGrid then
        Element.viewport Style.stylesheet <|
            Element.column Style.Main
                [ Attributes.height Attributes.fill
                , Attributes.width Attributes.fill
                ]
                [ navBar model
                , body model
                , footer model
                ]
    else
        Element.viewport Style.stylesheet <|
            Element.column Style.Main
                [ Attributes.height Attributes.fill ]
                [ navBar model
                , Element.el Style.None [ Attributes.height Attributes.fill ] <| Element.text "Oops kan met deze browser de inhoud niet weergeven"
                , footer model
                ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize
