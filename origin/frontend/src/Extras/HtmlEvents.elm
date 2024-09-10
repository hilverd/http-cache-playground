module Extras.HtmlEvents exposing
    ( KeyDownEvent
    , KeyValue
    , onEnter
    )

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode


type KeyValue
    = Character Char
    | Control String


type alias KeyDownEvent =
    { keyValue : KeyValue
    , controlKey : Bool
    , metaKey : Bool -- on macOS, this is the ⌘ key
    }


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


detailedKeyDownEventDecoder :
    (KeyDownEvent -> Maybe msg)
    -> Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
detailedKeyDownEventDecoder f =
    Decode.map3 KeyDownEvent
        (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
        (Decode.field "ctrlKey" <| Decode.bool)
        (Decode.field "metaKey" <| Decode.bool)
        |> Decode.andThen
            (\code ->
                case f code of
                    Just msg ->
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    Nothing ->
                        Decode.fail "no message for key"
            )


onKeydown : (KeyDownEvent -> Maybe msg) -> Attribute msg
onKeydown =
    Html.Events.custom "keydown" << detailedKeyDownEventDecoder


withoutModifiers : KeyValue -> KeyDownEvent
withoutModifiers keyValue =
    { keyValue = keyValue, controlKey = False, metaKey = False }


enter : KeyDownEvent
enter =
    Control "Enter" |> withoutModifiers


escape : KeyDownEvent
escape =
    Control "Escape" |> withoutModifiers


onEnter : msg -> Attribute msg
onEnter msg =
    onKeydown
        (\event ->
            if event == enter then
                Just msg

            else
                Nothing
        )
