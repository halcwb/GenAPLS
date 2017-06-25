module Util.Locals exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)


local_ n =
    Locale n "." ","


locale0 =
    local_ 0


locale1 =
    local_ 1


locale2 =
    local_ 2


local3 =
    local_ 3
