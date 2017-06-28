module Util.DomUtils exposing (..)

import Html exposing (Attribute, Html, button, div, input, p, text)
import Html.Attributes exposing (..)


createEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> a -> (a -> String) -> Html msg
createEl el txt model f =
    el [ style [ ( "padding", "5px" ) ] ] [ text (txt ++ (model |> f)) ]


createDiv : String -> a -> (a -> String) -> Html msg
createDiv =
    createEl div


createP : String -> a -> (a -> String) -> Html msg
createP =
    createEl p


createTr : String -> a -> (a -> String) -> Html msg
createTr txt model print =
    Html.tr [] [ createEl Html.td txt model print ]


createTr2 : String -> a -> (a -> String) -> Html msg
createTr2 txt x print =
    Html.tr []
        [ createEl Html.td txt x (\_ -> "")
        , createEl Html.td "" x print
        ]


createTr3 : String -> String -> a -> (a -> String) -> Html msg
createTr3 cat txt x print =
    Html.tr []
        [ createEl Html.td cat x (\_ -> "")
        , createEl Html.td txt x (\_ -> "")
        , createEl Html.td "" x print
        ]
