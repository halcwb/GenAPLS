module Util.DomUtils exposing (..)

import VirtualDom
import Json.Encode exposing (string)

import Html exposing (Attribute, Html, button, div, input, p, text)
import Html.Attributes exposing (..)

-- createEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> a -> (a -> String) -> Html msg



stylesheetLink : String -> Html msg
stylesheetLink url =
    VirtualDom.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []


