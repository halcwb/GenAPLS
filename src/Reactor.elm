module Reactor exposing (..)

import Main
import Navigation


-- Program


main : Program Never Main.Model Main.Msg
main =
    Navigation.program Main.UrlChange
        { init = Main.init { size = { width = 350, height = 826 }, userAgent = "Reactor", supportsGrid = True }
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
