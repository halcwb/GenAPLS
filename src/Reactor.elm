module Reactor exposing (..)

import Main
import Navigation


-- Program


main : Program Never Main.Model Main.Msg
main =
    Navigation.program Main.UrlChange
        { init = Main.init { size = { width = 1200, height = 826 }, userAgent = "Reactor", supportsGrid = False }
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
