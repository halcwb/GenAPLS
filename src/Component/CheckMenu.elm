module Component.CheckMenu exposing (Model, update, view)

import Html exposing (Html, text)
import Material.Icon as Icon
import Material.Dropdown.Item as Item
import Material.Options as Options
import Util.Utils exposing (eqs)


-- Model


type alias Model =
    { all : String
    , items : List String
    , selected : List String
    }



-- Update


update : String -> Model -> Model
update s model =
    if s == model.all then
        { model | selected = [] }
    else if model.selected |> List.any (eqs s) then
        { model | selected = model.selected |> List.filter (\x -> not (x == s)) }
    else
        { model | selected = model.selected |> List.append [ s ] }



-- View


view :
    (String -> msg)
    -> Model
    -> List (Item.Model msg)
view msg model =
    model.items
        |> List.map
            (\x ->
                Item.item
                    [ Item.onSelect (msg x) ]
                    [ checkmark (model.selected |> List.any (eqs x)), text x ]
            )
        |> List.append
            [ Item.item
                [ Item.onSelect (msg model.all) ]
                [ checkmark (model.selected |> List.isEmpty), text model.all ]
            ]


checkmark : Bool -> Html m
checkmark x =
    if x then
        Icon.view "check" [ Options.css "width" "40px" ]
    else
        Options.span [ Options.css "width" "40px" ] []
