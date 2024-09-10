module Extras.HtmlAttribute exposing (showIf, showUnless)

import Html
import Html.Attributes


empty : Html.Attribute msg
empty =
    Html.Attributes.classList []


showIf : Bool -> Html.Attribute msg -> Html.Attribute msg
showIf condition attribute =
    if condition then
        attribute

    else
        empty


showUnless : Bool -> Html.Attribute msg -> Html.Attribute msg
showUnless =
    not >> showIf
