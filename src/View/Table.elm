module View.Table
    exposing
        ( table
        , Msg
        , TableType(..)
        , Model
        )

import GenStyle as Style
import Element as Element
import Element.Events as Events
import Util.ListUtils as List


type Msg
    = TableRowEnter Int
    | TableRowLeave


type alias Model variation msg =
    { hoverRowIndx : Int
    , rows : List (List (Element.Element Style.Styles variation msg))
    }


type TableType
    = ElementTable


update msg model =
    case msg of
        TableRowEnter indx ->
            { model | hoverRowIndx = indx }

        TableRowLeave ->
            { model | hoverRowIndx = 0 }


elementTable model =
    let
        tableCell el indx =
            let
                style =
                    if model.hoverRowIndx == indx + 1 then
                        Style.TableRowHover
                    else
                        Style.TableRow
            in
                Element.el
                    style
                    [ Events.onMouseEnter (TableRowEnter (indx + 1))
                    , Events.onMouseLeave (TableRowLeave)
                    ]
                    el

        columns =
            (case model.rows of
                [] ->
                    []

                h :: t ->
                    h :: (t |> List.map (List.mapi tableCell))
            )
                |> List.transpose
    in
        Element.table Style.Main
            []
            columns


table tableType model msg =
    case tableType of
        --            HtmlTable ->
        ElementTable ->
            elementTable model
                |> Element.map msg
