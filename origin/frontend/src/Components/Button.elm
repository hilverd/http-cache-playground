module Components.Button exposing (text)

import Accessibility exposing (Attribute)
import Html exposing (Html)
import Html.Attributes exposing (class)


withAdditionalAttributes :
    List (Attribute msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
withAdditionalAttributes attributes additionalAttributes children =
    Accessibility.button
        ([ Html.Attributes.type_ "button"
         , class "select-none whitespace-nowrap"
         ]
            ++ (attributes ++ additionalAttributes)
        )
        children


text : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
text enabled =
    withAdditionalAttributes
        [ class "inline-flex items-center space-x-2 font-medium text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-gray-400 disabled:hover:cursor-not-allowed"
        , Html.Attributes.disabled <| not enabled
        ]
