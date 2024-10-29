module Extras.BrowserDom exposing (scrollElementIntoView, scrollToTop)

import Browser.Dom as Dom
import Task


scrollToTop : msg -> Cmd msg
scrollToTop noOpMsg =
    Dom.setViewport 0 0
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)


scrollElementIntoView : msg -> String -> Cmd msg
scrollElementIntoView noOpMsg id =
    id
        |> Dom.getElement
        |> Task.andThen (\element -> Dom.setViewport 0 <| element.element.y - 10)
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)
