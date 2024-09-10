module Components.RangeSlider exposing (viewSeconds)

import Dict exposing (Dict)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class)
import Html.Events


viewSeconds : Bool -> List Int -> (Int -> msg) -> Int -> Html msg
viewSeconds enabled range onInput value =
    let
        inputValueToValue : Dict String Int
        inputValueToValue =
            range
                |> List.indexedMap (String.fromInt >> Tuple.pair)
                |> Dict.fromList

        valueToInputValue : Dict Int String
        valueToInputValue =
            range
                |> List.indexedMap (\index v -> ( v, String.fromInt index ))
                |> Dict.fromList
    in
    div
        [ class "ml-4 max-w-48" ]
        [ input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max <| String.fromInt <| List.length range - 1
            , Html.Attributes.value <| Maybe.withDefault "0" <| Dict.get value valueToInputValue
            , Html.Attributes.step "1"
            , Html.Events.onInput (\v -> onInput <| Maybe.withDefault 0 <| Dict.get v inputValueToValue)
            , Html.Attributes.disabled <| not enabled
            , class "range [--range-shdw:gray]"
            ]
            []
        , div
            [ Html.Attributes.class "flex w-full justify-between px-2 select-none" ]
            (List.map (String.fromInt >> (\v -> span [] [ text v ])) range)
        ]
