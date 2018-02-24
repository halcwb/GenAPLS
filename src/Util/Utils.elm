module Util.Utils exposing (parseParams)

import Dict exposing (Dict)
import Http


--- UrlParser


parseParams : String -> Dict String String
parseParams queryString =
    queryString
        |> String.dropLeft 1
        |> String.split "&"
        |> List.filterMap toKeyValuePair
        |> Dict.fromList


toKeyValuePair : String -> Maybe ( String, String )
toKeyValuePair segment =
    case String.split "=" segment of
        [ key, value ] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

        _ ->
            Nothing
