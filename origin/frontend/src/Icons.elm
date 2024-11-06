module Icons exposing (chevronLeft, chevronRight, circleCheck, circleX, gitHubLogo, hourglass, logo, play, plus, repeat, trash)

import Html exposing (Html)
import Svg exposing (circle, path, svg)
import Svg.Attributes
    exposing
        ( clipRule
        , cx
        , cy
        , d
        , fill
        , fillRule
        , r
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeMiterlimit
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


circleX : List (Html.Attribute msg) -> Html msg
circleX =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ circle [ cx "12", cy "12", r "10" ] []
        , path [ d "m15 9-6 6" ] []
        , path [ d "m9 9 6 6" ] []
        ]


circleCheck : List (Html.Attribute msg) -> Html msg
circleCheck =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ circle [ cx "12", cy "12", r "10" ] []
        , path [ d "m9 12 2 2 4-4" ] []
        ]


logo : List (Html.Attribute msg) -> Html msg
logo =
    withAdditionalAttributes
        [ fill "none"
        , strokeLinecap "square"
        , strokeMiterlimit "10"
        , viewBox "0 0 257.35 257.35"
        ]
        [ path
            [ fill "currentColor"
            , d "M103.49 154.2c-26.518-2.599-42.462-5.546-59.347-10.971C15.628 134.069.523 121.332.5 106.428c-.012-7.294 2.558-13.039 8.052-18.002 4.358-3.937 6.025-4.334 8.367-1.992 2.195 2.195 2.02 2.917-1.752 7.176-5.302 5.987-6.676 8.473-6.676 12.074 0 8.423 7.306 15.979 22.5 23.269 53.99 25.904 165.54 22.564 206.52-6.184 12.829-9 14.843-18.471 5.922-27.848-4.848-5.097-5.221-9.351-.864-9.858 1.981-.23 3.56.766 6.826 4.31 8.316 9.022 10.04 17.2 5.531 26.246-11.906 23.893-64.031 39.645-129.33 39.084-10.233-.089-20.18-.315-22.105-.504zm-28.934-25.758c-.845-1.018-1.092-7.328-.872-22.277l.306-20.856 3.292-6.681C81.1 70.88 84.738 67.483 98.01 59.272c5.215-3.226 9.48-6.45 9.48-7.163 0-2.606-5.6-3.247-28.383-3.247-25.943 0-31.187.855-35.987 5.865-3.915 4.086-4.627 8.326-4.537 27.014.065 13.529-.19 16.648-1.47 17.927-1.256 1.256-2.027 1.33-4.086.393l-2.536-1.156.006-18.771c.003-10.324.486-20.505 1.075-22.623 1.508-5.433 8.338-11.913 15.3-14.518 5.291-1.98 7.594-2.13 32.55-2.13 23.626 0 26.929-.192 27.466-1.59.335-.875.38-1.961.101-2.414s-4.994-3.595-10.476-6.983c-12.188-7.532-14.384-7.657-14.824-.843-.367 5.695-2.194 7.608-5.728 5.998-2.22-1.012-2.47-1.722-2.47-7.012 0-4.388.513-6.54 2.015-8.45 2.425-3.081 7.963-5.09 11.744-4.26 1.515.333 8.813 4.417 16.218 9.077 7.406 4.659 14.153 8.285 14.994 8.058 4.532-1.225 11.191-1.484 16.311-.635l5.783.96 11.019-6.96c18.439-11.644 20.268-12.296 26.37-9.401 4.36 2.069 5.902 5.31 5.788 12.164-.113 6.786-2.143 9.26-5.68 6.922-1.552-1.026-2.137-2.632-2.364-6.497-.275-4.666-.521-5.134-2.698-5.134-1.318 0-6.807 2.755-12.198 6.124-12.024 7.512-11.924 7.421-10.337 9.333 1.094 1.318 5.09 1.543 27.476 1.543 30.306 0 34.053.648 41.059 7.106 7.165 6.605 7.5 7.975 7.5 30.672 0 16.71-.25 20.184-1.53 21.246-1.17.97-2.11 1.005-4 .144l-2.47-1.126V79.752c0-21.916-.63-24.042-8.35-28.17-3.957-2.116-5.393-2.229-30.873-2.425-22.89-.177-26.943.014-28.259 1.33-.845.845-1.323 1.882-1.062 2.304.26.422 4.652 3.378 9.759 6.57 12.093 7.558 17.963 13.278 21.334 20.79 2.66 5.926 2.695 6.292 2.593 26.485-.106 20.917-.51 23.226-4.064 23.226-3.741 0-4.078-1.835-4.078-22.21 0-22.65-.628-25.119-8.273-32.5-5.683-5.487-25.242-17.29-28.65-17.29-5.85 0-6.03.64-6.316 22.5l-.261 20-2.305.327c-4.897.694-5.195-.566-5.195-21.93 0-21.959.574-19.939-6-21.107-2.34-.416-23.966 13.174-29.175 18.334-6.823 6.759-7.825 11.03-7.825 33.358 0 18.618-.378 20.518-4.078 20.518-.923 0-2.208-.64-2.857-1.42zm62.633-79.082c6.209-1.844 8.016-3.64 5.845-5.81-5.668-5.668-29.544-4.307-29.544 1.684 0 1.275 5.349 4.071 9.5 4.966 4.216.909 9.328.606 14.199-.84z"
            ]
            []
        , path
            [ stroke "currentColor"
            , strokeLinecap "butt"
            , strokeLinejoin "round"
            , strokeWidth "8"
            , d "M33.47 160.3a6.913 6.913 0 1 1 13.827 0 6.913 6.913 0 0 1-13.827 0z"
            ]
            []
        , path
            [ fill "currentColor"
            , d "M103.49 200.78c-26.518-2.599-42.462-5.546-59.347-10.971C15.628 180.649.523 167.912.5 153.008c-.011-7.294-2.335-32.21 3.159-37.173 4.358-3.937 7.973 4.048 7.655 7.345 0 0 .874 1.388-.509 5.278-2.679 7.536-2.314 20.205-2.314 23.806 0 8.423 7.306 15.979 22.5 23.269 53.99 25.904 165.54 22.564 206.52-6.184 12.829-8.999 13.74-24.554 10.23-37.011-2.358-8.366-.232-13.524 4.125-14.031 1.981-.23-.954-4.858 1.382 8.661 2.09 12.092 6.187 26.184 1.678 35.232-11.906 23.892-64.03 39.644-129.33 39.083-10.233-.088-20.18-.314-22.105-.503z"
            ]
            []
        , path
            [ stroke "currentColor"
            , strokeLinecap "round"
            , strokeWidth "6.3"
            , d "M63.42 165.5s33.78 13.203 88.696 8.855c58.658-4.645 81.665-21.494 81.665-21.494"
            ]
            []
        , path
            [ stroke "currentColor"
            , strokeLinecap "butt"
            , strokeLinejoin "round"
            , strokeWidth "8"
            , d "M33.47 206.782a6.913 6.913 0 1 1 13.827 0 6.913 6.913 0 0 1-13.827 0z"
            ]
            []
        , path
            [ fill "currentColor"
            , d "M103.49 247.262c-26.518-2.599-42.462-5.546-59.347-10.971C15.628 227.131.523 214.394.5 199.49c-.011-7.294-2.335-32.21 3.159-37.173 4.358-3.937 7.973 4.048 7.655 7.345 0 0 .874 1.388-.509 5.278-2.679 7.536-2.314 20.205-2.314 23.806 0 8.423 7.306 15.979 22.5 23.269 53.99 25.904 165.54 22.564 206.52-6.184 12.829-8.999 13.74-24.554 10.23-37.011-2.358-8.366-.232-13.524 4.125-14.031 1.981-.23-.954-4.858 1.382 8.661 2.09 12.092 6.187 26.184 1.678 35.232-11.906 23.892-64.03 39.644-129.33 39.083-10.233-.088-20.18-.314-22.105-.503z"
            ]
            []
        , path
            [ stroke "currentColor"
            , strokeLinecap "round"
            , strokeWidth "6.3"
            , d "M63.42 211.982s33.78 13.203 88.696 8.855c58.658-4.645 81.665-21.494 81.665-21.494"
            ]
            []
        ]
