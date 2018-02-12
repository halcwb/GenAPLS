module Util.DomUtils exposing (..)

import VirtualDom
import Json.Encode exposing (string)

import Html exposing (Attribute, Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Material.Table as Table
import Material.Options exposing (cs, css)

-- createEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> a -> (a -> String) -> Html msg


-- CREATE ELEMENT

createEl el x f =
    el [ cs "mdl-data-table__cell--non-numeric"  ] [ text (x |> f) ]



-- CREATE TABLE HEADER

--createTh4 : String -> String -> String -> String -> Html msg
createTh4 s1 s2 s3 s4 =
    Table.thead []
        [ createEl Table.th s1 identity
        , createEl Table.th s2 identity
        , createEl Table.th s3 identity
        , createEl Table.th s4 identity
        ]

stylesheetLink : String -> Html msg
stylesheetLink url =
    VirtualDom.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []


