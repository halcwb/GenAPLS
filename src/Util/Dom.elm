module Util.Dom exposing (..)

import Html exposing (Html, text, div, p, button, input, Attribute)
import Html.Attributes exposing (..)


createEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> a -> (a -> String) -> Html msg
createEl el txt model f =
    el [] [ text (txt ++ (model |> f)) ]


createDiv : String -> a -> (a -> String) -> Html msg
createDiv =
    createEl div


createP : String -> a -> (a -> String) -> Html msg
createP =
    createEl p
