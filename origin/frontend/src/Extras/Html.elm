module Extras.Html exposing (externalLink, nothing, showIf, showMaybe, showUnless)

import Html exposing (Html, a)
import Html.Attributes exposing (class)


nothing : Html msg
nothing =
    Html.text ""


showIf : Bool -> Html msg -> Html msg
showIf condition html =
    if condition then
        html

    else
        nothing


showUnless : Bool -> Html msg -> Html msg
showUnless =
    not >> showIf


showMaybe : (a -> Html msg) -> Maybe a -> Html msg
showMaybe f =
    Maybe.map f
        >> Maybe.withDefault nothing


externalLink : String -> List (Html msg) -> Html msg
externalLink href body =
    a
        [ class "font-semibold leading-6 text-indigo-400 hover:underline"
        , Html.Attributes.target "_blank"
        , Html.Attributes.rel "noopener noreferrer"
        , Html.Attributes.href href
        ]
        body
