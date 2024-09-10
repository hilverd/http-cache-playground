module Data.CacheControlResponseDirectives exposing (CacheControlResponseDirectives, empty, fromString, maxAge, noStore, private, sMaxAge, staleWhileRevalidate, toString, updateMaxAge, updateNoStore, updatePrivate, updateSMaxAge, updateStaleWhileRevalidate)


type CacheControlResponseDirectives
    = CacheControlResponseDirectives
        { maxAge : Maybe Int
        , sMaxAge : Maybe Int
        , noStore : Bool
        , private : Bool
        , staleWhileRevalidate : Maybe Int
        }


empty : CacheControlResponseDirectives
empty =
    CacheControlResponseDirectives
        { maxAge = Nothing
        , sMaxAge = Nothing
        , noStore = False
        , private = False
        , staleWhileRevalidate = Nothing
        }


maxAge : CacheControlResponseDirectives -> Maybe Int
maxAge (CacheControlResponseDirectives directives) =
    directives.maxAge


sMaxAge : CacheControlResponseDirectives -> Maybe Int
sMaxAge (CacheControlResponseDirectives directives) =
    directives.sMaxAge


noStore : CacheControlResponseDirectives -> Bool
noStore (CacheControlResponseDirectives directives) =
    directives.noStore


private : CacheControlResponseDirectives -> Bool
private (CacheControlResponseDirectives directives) =
    directives.private


staleWhileRevalidate : CacheControlResponseDirectives -> Maybe Int
staleWhileRevalidate (CacheControlResponseDirectives directives) =
    directives.staleWhileRevalidate


updateMaxAge : Maybe Int -> CacheControlResponseDirectives -> CacheControlResponseDirectives
updateMaxAge maxAge_ (CacheControlResponseDirectives directives) =
    CacheControlResponseDirectives { directives | maxAge = maxAge_ }


updateNoStore : Bool -> CacheControlResponseDirectives -> CacheControlResponseDirectives
updateNoStore noStore_ (CacheControlResponseDirectives directives) =
    CacheControlResponseDirectives { directives | noStore = noStore_ }


updatePrivate : Bool -> CacheControlResponseDirectives -> CacheControlResponseDirectives
updatePrivate private_ (CacheControlResponseDirectives directives) =
    CacheControlResponseDirectives { directives | private = private_ }


updateStaleWhileRevalidate : Maybe Int -> CacheControlResponseDirectives -> CacheControlResponseDirectives
updateStaleWhileRevalidate staleWhileRevalidate_ (CacheControlResponseDirectives directives) =
    CacheControlResponseDirectives { directives | staleWhileRevalidate = staleWhileRevalidate_ }


updateSMaxAge : Maybe Int -> CacheControlResponseDirectives -> CacheControlResponseDirectives
updateSMaxAge sMaxAge_ (CacheControlResponseDirectives directives) =
    CacheControlResponseDirectives { directives | sMaxAge = sMaxAge_ }


fromString : String -> CacheControlResponseDirectives
fromString string =
    let
        processDirective : String -> CacheControlResponseDirectives -> CacheControlResponseDirectives
        processDirective directive directives =
            if String.startsWith "max-age=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                updateMaxAge
                    (seconds |> String.toInt |> Maybe.withDefault 0 |> Just)
                    directives

            else if directive == "no-store" then
                updateNoStore True directives

            else if directive == "private" then
                updatePrivate True directives

            else if String.startsWith "stale-while-revalidate=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                updateStaleWhileRevalidate
                    (seconds |> String.toInt |> Maybe.withDefault 0 |> Just)
                    directives

            else if String.startsWith "s-maxage=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                updateSMaxAge
                    (seconds |> String.toInt |> Maybe.withDefault 0 |> Just)
                    directives

            else
                directives
    in
    string
        |> String.split ","
        |> List.map String.trim
        |> List.foldl processDirective empty


toString : CacheControlResponseDirectives -> String
toString (CacheControlResponseDirectives directives) =
    [ directives.maxAge
        |> Maybe.map (\maxAge_ -> "max-age=" ++ String.fromInt maxAge_)
    , directives.sMaxAge
        |> Maybe.map (\sMaxAge_ -> "s-maxage=" ++ String.fromInt sMaxAge_)
    , if directives.noStore then
        Just "no-store"

      else
        Nothing
    , if directives.private then
        Just "private"

      else
        Nothing
    , directives.staleWhileRevalidate
        |> Maybe.map
            (\staleWhileRevalidate_ ->
                "stale-while-revalidate=" ++ String.fromInt staleWhileRevalidate_
            )
    ]
        |> List.filterMap identity
        |> String.join ","
