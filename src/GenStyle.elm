module GenStyle
    exposing
        ( Styles (..)
        , stylesheet
        )

import Style as Style
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border
import Style.Shadow as Shadow
import MaterialColor as Color
import Color


-- Style


type Styles
    = None
    | Main
    | Header
    | Input
    | Label
    | Select
    | Button
    | TableHead
    | TableRow
    | TableRowHover
    | Title
    | MenuContents
    | MenuItem
    | MenuItemSelected
    | Footer


roboto : List Style.Font
roboto =
    [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } ]


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style Main
            [ Font.typeface roboto
            ]
        , Style.style Header
            [ Color.text Color.white
            , Color.background Color.teal800
            , Font.size 40
            ]
        , Style.style Button
            [ Border.rounded 5
            , Color.text Color.white
            , Color.background Color.teal900
            , Font.size 14
            , Font.bold
            , Style.hover [ Shadow.simple ]
            ]
        , Style.style Input
            [ Font.size 16
            , Color.text Color.teal900
            , Color.border Color.teal100
            , Border.bottom 1
            , Style.hover [ Shadow.simple ]
            , Style.focus [ Shadow.simple ]
            ]
        , Style.style Select
            [ Color.border Color.teal100
            , Border.bottom 1
            , Style.hover [ Shadow.simple ]
            ]
        , Style.style Label
            [ Color.text Color.teal400
            , Font.size 14
            ]
        , Style.style TableHead
            [ Color.text <| Color.rgb 158 158 158
            , Font.size 13
            , Font.bold
            , Border.bottom 2
            , Color.border <| Color.lightGray
            , Style.cursor "pointer"
            ]
        , Style.style TableRow
            [ Color.text <| Color.rgb 117 117 117

            --             Only hovers a cell not the whole row
            --             , Style.hover [ Color.background Color.lightGray
            --                           ]
            , Font.size 14
            , Border.bottom 1
            , Color.border <| Color.lightGray
            ]
        , Style.style TableRowHover
            [ Font.size 14
            , Color.text Color.black
            , Color.background Color.lightGray
            ]
        , Style.style Title
            [ Color.text <| Color.teal400
            , Font.size 16
            , Font.bold
            ]
        , Style.style MenuContents
            [ Color.background Color.white
            , Color.border Color.lightGray
            , Border.all 1
            , Shadow.simple
            ]
        , Style.style MenuItem
            [ Style.cursor "pointer"
            , Style.hover
                [ Color.background Color.lightGray
                ]
            , Color.text Color.black
            , Font.light
            ]
        , Style.style MenuItemSelected
            [ Style.cursor "pointer"
            , Color.background Color.lightGray
            , Color.text Color.black
            , Font.light
            ]
        , Style.style Footer
            [ Color.background <| Color.rgb 66 66 66
            , Color.text Color.white
            ]
        ]
