module Util.ElementUtils exposing (PaddingType(..), responsivePadding)

import Element as Element
import Element.Attributes as Attributes


type PaddingType
    = All
    | Left
    | Right
    | Bottom
    | Top


responsivePadding : PaddingType -> Float -> Float -> Float -> Element.Attribute variation msg
responsivePadding pt w min max =
    let
        p =
            Element.responsive w ( 320, 2560 ) ( min, max )
    in
        case pt of
            All ->
                Attributes.padding p

            Left ->
                Attributes.paddingLeft p

            Right ->
                Attributes.paddingRight p

            Bottom ->
                Attributes.paddingTop p

            Top ->
                Attributes.paddingBottom p
