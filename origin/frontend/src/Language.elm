module Language exposing (seconds, secondsFromString)


seconds : Int -> String
seconds n =
    case n of
        1 ->
            "1 second"

        _ ->
            String.fromInt n ++ " seconds"


secondsFromString : String -> String
secondsFromString value =
    value
        |> String.toInt
        |> Maybe.map seconds
        |> Maybe.withDefault value
