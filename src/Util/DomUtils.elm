module Util.DomUtils exposing (..)

import VirtualDom
import Json.Encode exposing (string)

import Html exposing (Attribute, Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Material.Table as Table
import Material.Options exposing (cs, css)

-- createEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> a -> (a -> String) -> Html msg


createEl el x f =
    el [ cs "mdl-data-table__cell--non-numeric"  ] [ text (x |> f) ]



createTr1 : a -> (a -> String) -> Html msg
createTr1 x f1 =
    Table.tr [] [ createEl Table.td x f1 ]


createTr2 : a -> (a -> String) -> (a -> String) -> Html msg
createTr2 x f1 f2 =
    Table.tr []
        [ createEl Table.td x f1
        , createEl Table.td x f2
        ]


createTr3 : a -> (a -> String) -> (a -> String) -> (a -> String) -> Html msg
createTr3 x f1 f2 f3 =
    Table.tr []
        [ createEl Table.td x f1
        , createEl Table.td x f2
        , createEl Table.td x f3
        ]


createTr4 : a -> (a -> String) -> (a -> String) -> (a -> String) -> (a -> String) -> Html msg
createTr4 x f1 f2 f3 f4 =
    Table.tr []
        [ createEl Table.td x f1
        , createEl Table.td x f2
        , createEl Table.td x f3
        , createEl Table.td x f4
        ]


createTr5 : a -> (a -> String) -> (a -> String) -> (a -> String) -> (a -> String) -> (a -> String) -> Html msg
createTr5 x f1 f2 f3 f4 f5 =
    Table.tr []
        [ createEl Table.td x f1
        , createEl Table.td x f2
        , createEl Table.td x f3
        , createEl Table.td x f4
        , createEl Table.td x f5
        ]


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


