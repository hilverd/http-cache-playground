module Extras.BrowserDom exposing (scrollToTop)

import Browser.Dom as Dom
import Task


scrollToTop : msg -> Cmd msg
scrollToTop noOpMsg =
    Dom.setViewport 0 0
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)
