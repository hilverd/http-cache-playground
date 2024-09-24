module Icons exposing (chevronLeft, chevronRight, gitHubLogo, graduationCap, hourglass, play, plus, repeat, trash)

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes
    exposing
        ( clipRule
        , d
        , fill
        , fillRule
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , viewBox
        )


withAdditionalAttributes :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> List (Html.Attribute msg)
    -> Html msg
withAdditionalAttributes attributes children additionalAttributes =
    svg (attributes ++ additionalAttributes) children


chevronRight : List (Html.Attribute msg) -> Html msg
chevronRight =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "3"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "m9 18 6-6-6-6" ]
            []
        ]


chevronLeft : List (Html.Attribute msg) -> Html msg
chevronLeft =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "3"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "m15 18-6-6 6-6" ]
            []
        ]


repeat : List (Html.Attribute msg) -> Html msg
repeat =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "m17 2 4 4-4 4" ] []
        , path [ d "M3 11v-1a4 4 0 0 1 4-4h14" ] []
        , path [ d "m7 22-4-4 4-4" ] []
        , path [ d "M21 13v1a4 4 0 0 1-4 4H3" ] []
        ]


hourglass : List (Html.Attribute msg) -> Html msg
hourglass =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M5 22h14" ] []
        , path [ d "M5 2h14" ] []
        , path [ d "M17 22v-4.172a2 2 0 0 0-.586-1.414L12 12l-4.414 4.414A2 2 0 0 0 7 17.828V22" ] []
        , path [ d "M7 2v4.172a2 2 0 0 0 .586 1.414L12 12l4.414-4.414A2 2 0 0 0 17 6.172V2" ] []
        ]


trash : List (Html.Attribute msg) -> Html msg
trash =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


plus : List (Html.Attribute msg) -> Html msg
plus =
    withAdditionalAttributes
        [ stroke "none"
        , fill "currentColor"
        , viewBox "0 0 20 20"
        ]
        [ path
            [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
            []
        ]


play : List (Html.Attribute msg) -> Html msg
play =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "M5.25 5.653c0-.856.917-1.398 1.667-.986l11.54 6.347a1.125 1.125 0 0 1 0 1.972l-11.54 6.347a1.125 1.125 0 0 1-1.667-.986V5.653Z" ]
            []
        ]


gitHubLogo : List (Html.Attribute msg) -> Html msg
gitHubLogo =
    withAdditionalAttributes
        [ fill "currentColor"
        , viewBox "0 0 16 16"
        ]
        [ path
            [ d "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z" ]
            []
        ]


graduationCap : List (Html.Attribute msg) -> Html msg
graduationCap =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "M21.42 10.922a1 1 0 0 0-.019-1.838L12.83 5.18a2 2 0 0 0-1.66 0L2.6 9.08a1 1 0 0 0 0 1.832l8.57 3.908a2 2 0 0 0 1.66 0z" ]
            []
        , path
            [ d "M22 10v6" ]
            []
        , path
            [ d "M6 12.5V16a6 3 0 0 0 12 0v-3.5" ]
            []
        ]
