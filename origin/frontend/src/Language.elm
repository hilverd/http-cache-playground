module Language exposing (seconds)


seconds : Int -> String
seconds n =
    case n of
        1 ->
            "1 second"

        _ ->
            String.fromInt n ++ " seconds"
