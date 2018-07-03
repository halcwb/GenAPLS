module Main exposing (..)

import GenStyle as Style
import Html exposing (Html)
import Navigation
import Element as Element
import Element.Attributes as Attributes
import Window
import Page.EmergencyList as EmergencyList exposing (..)
import Util.ElementUtils as Padding


-- Model


type alias Model =
    { emergencyList : EmergencyList.Model
    , device : Device
    }


type alias Device =
    { size : Window.Size
    , userAgent : String
    , supportsGrid : Bool
    }



-- Program


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

        -- EmergencyList Message
        --
        EListMsg msg ->
            ( { model
                | emergencyList =
                    model.emergencyList
                        |> EmergencyList.update msg
                        |> Tuple.first
              }
            , Cmd.none
            )

        -- Device Message
        --
        Resize size ->
            let
                device =
                    model.device
            in
                ( { model | device = { device | size = size } }, Cmd.none )



-- Navbar


navBar : Model -> Element.Element Style.Styles variation msg
navBar model =
    let
        title =
            "Pediatrische Noodlijst Berekeningen"
    in
        Element.header Style.Header
            [ responsivePadding model ]
            (Element.text title)



-- Footer


footer : Model -> Element.Element Style.Styles variation msg
footer model =
    Element.footer Style.Footer
        [ responsivePaddingLeft model
        , Attributes.paddingTop 10
        , Attributes.paddingBottom 10
        ]
        (Element.row Style.None
            [ Attributes.paddingTop 10
            , Attributes.paddingBottom 10
            , Attributes.spacing 20
            ]
            [ Element.link "http://github.com/halcwb/GenAPLS.git" <|
                Element.text "GenAPLS Informedica 2008"
            , Element.link "http://www.picuwkz.nl" <|
                Element.text "PICU WKZ"
            , Element.link "https://www.kinderformularium.nl" <|
                Element.text "Kinderformularium"
            , Element.text <|
                "Browser size: "
                    ++ (toString model.device.size.width)
                    ++ " x "
                    ++ (toString model.device.size.height)
            ]
        )



-- Body


body : Bool -> Model -> Element.Element Style.Styles variation Msg
body supportsGrid model =
    Element.column
        Style.None
        [ Attributes.center
        , Attributes.height Attributes.fill
        , responsivePaddingLeft model
        , responsivePaddingRight model
        ]
        [ model.device.size
            |> Element.classifyDevice
            |> EmergencyList.body supportsGrid model.emergencyList
            |> Element.map EListMsg
        ]



-- View


notSupported : Element.Element style variation msg
notSupported =
    Element.text "Oops kan met deze browser de inhoud niet weergeven"


view : Model -> Html Msg
view model =
    let
        stylesheet =
            Style.stylesheet model.device.size
    in
        Element.viewport stylesheet <|
            Element.column Style.Main
                [ Attributes.height Attributes.fill
                ]
                [ navBar model
                , body model.device.supportsGrid model
                , footer model
                ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize



-- Helper Functions


responsivePadding :
    { c | device : { b | size : { a | width : Int } } }
    -> Element.Attribute variation msg
responsivePadding model =
    Padding.responsivePadding Padding.All (toFloat model.device.size.width) 10 100


responsivePaddingLeft :
    { c | device : { b | size : { a | width : Int } } }
    -> Element.Attribute variation msg
responsivePaddingLeft model =
    Padding.responsivePadding Padding.Left (toFloat model.device.size.width) 10 100


responsivePaddingRight :
    { c | device : { b | size : { a | width : Int } } }
    -> Element.Attribute variation msg
responsivePaddingRight model =
    Padding.responsivePadding Padding.Right (toFloat model.device.size.width) 10 100
