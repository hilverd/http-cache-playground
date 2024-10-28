module Interactions exposing (Interactions, codec, empty, isEmpty, view, withoutInteractionsAfterFinalClientAction)

import Codec exposing (Codec)
import Data.Interaction as Interaction exposing (Interaction(..))
import Data.SequenceDiagramVisibility exposing (SequenceDiagramVisibility(..))
import Dict exposing (Dict)
import Extras.Html
import Extras.HtmlAttribute
import Html exposing (..)
import Html.Attributes exposing (class)
import Icons
import Language
import Svg.Attributes


type Interactions
    = Interactions (List Interaction)


codec : Codec Interactions
codec =
    Codec.map Interactions
        (\(Interactions interactionsList) -> interactionsList)
        (Codec.list Interaction.codec)


empty : Interactions
empty =
    Interactions []


isEmpty : Interactions -> Bool
isEmpty (Interactions interactions) =
    List.isEmpty interactions


withoutInteractionsAfterFinalClientAction : Int -> Interactions -> Interactions
withoutInteractionsAfterFinalClientAction stepIndexOfFinalClientAction (Interactions interactions) =
    interactions
        |> List.foldl
            (\interaction ( finalClientActionSeen, result ) ->
                if finalClientActionSeen then
                    ( finalClientActionSeen, result )

                else
                    case interaction of
                        ClientSleepingForSeconds stepIndex _ ->
                            if stepIndex == stepIndexOfFinalClientAction then
                                ( True, interaction :: result )

                            else
                                ( False, interaction :: result )

                        ClientToVarnish stepIndex _ ->
                            if stepIndex == stepIndexOfFinalClientAction then
                                ( True, interaction :: result )

                            else
                                ( False, interaction :: result )

                        _ ->
                            ( finalClientActionSeen, interaction :: result )
            )
            ( False, [] )
        |> Tuple.second
        |> List.reverse
        |> Interactions


type BodyColour
    = Blue
    | Yellow
    | Green


bodyToBodyColour : List Interaction -> Dict String BodyColour
bodyToBodyColour interactions =
    interactions
        |> List.filterMap
            (\interaction ->
                case interaction of
                    OriginToVarnish { body } ->
                        Just body

                    VarnishToClient { body } ->
                        Just body

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\body ( availableColours, result ) ->
                if Dict.member body result then
                    ( availableColours, result )

                else
                    case availableColours of
                        head :: tail ->
                            ( tail, Dict.insert body head result )

                        [] ->
                            ( [], result )
            )
            ( [ Blue, Yellow, Green ], Dict.empty )
        |> Tuple.second


bodyBadge : Maybe BodyColour -> String -> Html msg
bodyBadge colour body =
    if String.isEmpty body then
        text "[Empty body]"

    else
        span
            []
            [ text "Body: "
            , case colour of
                Just Blue ->
                    span
                        [ class "inline-flex items-center rounded-md bg-blue-100 px-2 py-1 font-medium text-blue-700" ]
                        [ text body ]

                Just Yellow ->
                    span
                        [ class "inline-flex items-center rounded-md bg-yellow-100 px-2 py-1 font-medium text-yellow-800" ]
                        [ text body ]

                Just Green ->
                    span
                        [ class "inline-flex items-center rounded-md bg-green-100 px-2 py-1 font-medium text-green-700" ]
                        [ text body ]

                Nothing ->
                    text body
            ]


view :
    { scenarioIsRunning : Bool
    , showAllHeaders : Bool
    , allRequestHeaderKeys : List String
    , allResponseHeaderKeys : List String
    , sequenceDiagramVisibility : SequenceDiagramVisibility
    }
    -> Interactions
    -> Html msg
view { scenarioIsRunning, showAllHeaders, allRequestHeaderKeys, allResponseHeaderKeys, sequenceDiagramVisibility } (Interactions interactions) =
    if List.isEmpty interactions then
        div
            [ class "text-gray-700 mt-4" ]
            [ text "[No interactions have happened yet.]" ]

    else
        let
            bodyToBodyColour_ =
                bodyToBodyColour interactions
        in
        div
            []
            [ div
                [ Html.Attributes.id "sequence-diagram" ]
                (div
                    [ class "grid grid-cols-8 text-center text-base sticky top-0 bg-white z-20 pt-8" ]
                    [ div
                        [ class "col-span-2 py-0.5" ]
                        [ span
                            [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 shadow-md" ]
                            [ text "Client" ]
                        ]
                    , div [] []
                    , div
                        [ class "col-span-2 py-0.5" ]
                        [ span
                            [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 shadow-md" ]
                            [ text "Varnish" ]
                        ]
                    , div [] []
                    , div
                        [ class "col-span-2 py-0.5" ]
                        [ span
                            [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 shadow-md" ]
                            [ text "Origin" ]
                        ]
                    ]
                    :: (List.map (viewInteraction showAllHeaders allRequestHeaderKeys allResponseHeaderKeys bodyToBodyColour_) interactions
                            ++ (if not scenarioIsRunning && sequenceDiagramVisibility /= CompletelyRevealed && sequenceDiagramVisibility /= FinalInteractionsRevealedForExercise then
                                    [ viewSpacer
                                    , viewDashedSpacer
                                    ]

                                else
                                    []
                               )
                            ++ (if scenarioIsRunning then
                                    viewAgentsAtBottom scenarioIsRunning

                                else
                                    case sequenceDiagramVisibility of
                                        FinalInteractionsConcealedForExercise ->
                                            []

                                        FinalInteractionsRevealedForExercise ->
                                            viewAgentsAtBottom scenarioIsRunning

                                        CompletelyRevealed ->
                                            viewAgentsAtBottom scenarioIsRunning
                               )
                       )
                )
            ]


viewAgentsAtBottom : Bool -> List (Html msg)
viewAgentsAtBottom scenarioIsRunning =
    [ viewSpacer
    , div
        [ class "pb-8 grid grid-cols-8 text-center text-base sticky bottom-0 bg-white z-20" ]
        [ div
            [ class "col-span-2 py-0.5"
            , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-bounce"
            ]
            [ span
                [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 bg-white shadow-md"
                , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-pulse"
                ]
                [ text "Client" ]
            ]
        , div [] []
        , div
            [ class "col-span-2 py-0.5"
            , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-bounce"
            ]
            [ span
                [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 bg-white shadow-md"
                , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-pulse"
                ]
                [ text "Varnish" ]
            ]
        , div [] []
        , div
            [ class "col-span-2 py-0.5"
            , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-bounce"
            ]
            [ span
                [ class "border-2 rounded-md border-gray-500 px-2 py-1 text-gray-800 bg-white shadow-md"
                , Extras.HtmlAttribute.showIf scenarioIsRunning <| class "motion-safe:animate-pulse"
                ]
                [ text "Origin" ]
            ]
        ]
    ]


viewSpacer : Html msg
viewSpacer =
    div
        [ class "grid grid-cols-8" ]
        [ div [] []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        , div [] [ text "\u{00A0}" ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewDashedSpacer : Html msg
viewDashedSpacer =
    div
        [ class "grid grid-cols-8" ]
        [ div [] []
        , div
            [ class "col-span-3 border-dashed border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "col-span-3 border-dashed border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-dashed border-l-gray-300" ]
            []
        , div
            [ class "py-2" ]
            [ text "\u{00A0}" ]
        , div
            [ class "col-span-3 border-dashed border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "col-span-3 border-dashed border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-dashed border-l-gray-300" ]
            []
        ]


viewInteraction : Bool -> List String -> List String -> Dict String BodyColour -> Interaction -> Html msg
viewInteraction showAllHeaders extraKeysOfRequestHeadersToShow extraKeysOfResponseHeadersToShow bodyToBodyColour_ interaction =
    case interaction of
        ClientSleepingForSeconds stepIndex seconds ->
            viewClientSleepingForSeconds stepIndex seconds

        ClientToVarnish stepIndex { method, path, headers } ->
            viewClientToVarnishInteraction showAllHeaders stepIndex method path extraKeysOfRequestHeadersToShow headers

        VarnishToOrigin { path, headers } ->
            viewVarnishToOriginInteraction showAllHeaders path extraKeysOfRequestHeadersToShow headers

        OriginSleepingForSeconds seconds ->
            viewOriginSleepingForSeconds seconds

        OriginToVarnish { statusCode, headers, body } ->
            viewOriginToVarnishInteraction showAllHeaders bodyToBodyColour_ statusCode extraKeysOfResponseHeadersToShow headers body

        VarnishToClient { statusCode, headers, body } ->
            viewVarnishToClientInteraction showAllHeaders bodyToBodyColour_ statusCode extraKeysOfResponseHeadersToShow headers body


viewClientSleepingForSeconds : Int -> Int -> Html msg
viewClientSleepingForSeconds stepIndex seconds =
    div
        [ class "grid grid-cols-8" ]
        [ div
            [ class "pt-6 text-right border-b-2 border-b-white self-start" ]
            [ span
                [ class "badge badge-ghost text-base mr-3" ]
                [ span
                    [ class "hidden sm:inline sm:mr-1" ]
                    [ text "Step" ]
                , text <| String.fromInt <| stepIndex + 1
                ]
            ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 px-2 pt-4" ]
            [ span
                [ class "-ml-2 my-2 inline-flex items-center text-gray-700 z-10" ]
                [ Icons.hourglass
                    [ Svg.Attributes.class "w-5 h-5 -ml-3" ]
                , span
                    [ class "ml-3" ]
                    [ text <| "Sleep " ++ Language.seconds seconds ]
                ]
            ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewOriginSleepingForSeconds : Int -> Html msg
viewOriginSleepingForSeconds seconds =
    div
        [ class "grid grid-cols-8 " ]
        [ div [] []
        , div
            [ class "border-l-2 border-l-gray-300 col-span-3" ]
            []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 p-2" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300 pt-4" ]
            [ span
                [ class "-ml-2 my-2 inline-flex items-center text-gray-700 z-10" ]
                [ Icons.hourglass
                    [ Svg.Attributes.class "w-5 h-5 -ml-1 shrink-0" ]
                , span
                    [ class "ml-3" ]
                    [ text <| "Sleep " ++ Language.seconds seconds ]
                ]
            ]
        ]


viewClientToVarnishInteraction : Bool -> Int -> String -> String -> List String -> List ( String, String ) -> Html msg
viewClientToVarnishInteraction showAllHeaders stepIndex method path extraKeysOfHeadersToShow headers =
    div
        [ class "grid grid-cols-8" ]
        [ div
            [ class "pt-4 text-right border-b-2 border-b-white self-start" ]
            [ span
                [ class "badge badge-ghost text-base mr-3" ]
                [ span
                    [ class "hidden sm:inline sm:mr-1" ]
                    [ text "Step" ]
                , text <| String.fromInt <| stepIndex + 1
                ]
            ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 border-b-2 border-b-gray-700 p-2 pt-4 relative text-gray-700" ]
            [ div
                [ class "font-mono text-ellipsis text-nowrap overflow-hidden max-w-44" ]
                [ text <| method ++ " " ++ path ]
            , ul
                [ class "list-disc list-inside mt-2 space-y-1" ]
                (viewRequestHeaders showAllHeaders extraKeysOfHeadersToShow headers)
            , span
                [ class "absolute right-0 z-10" ]
                [ Icons.chevronRight
                    [ Svg.Attributes.class "w-5 h-5 -mr-2" ]
                ]
            ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewVarnishToOriginInteraction : Bool -> String -> List String -> List ( String, String ) -> Html msg
viewVarnishToOriginInteraction showAllHeaders path extraKeysOfHeadersToShow headers =
    div
        [ class "grid grid-cols-8" ]
        [ div [] []
        , div
            [ class "border-l-2 border-l-gray-300 col-span-3" ]
            []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 border-b-2 border-b-gray-700 p-2 pt-4 relative text-gray-700" ]
            [ div
                [ class "font-mono text-ellipsis text-nowrap overflow-hidden max-w-44" ]
                [ text <| "GET " ++ path ]
            , ul
                [ class "list-disc list-inside mt-2 space-y-1" ]
                (viewRequestHeaders showAllHeaders extraKeysOfHeadersToShow headers)
            , span
                [ class "absolute right-0 z-10" ]
                [ Icons.chevronRight
                    [ Svg.Attributes.class "w-5 h-5 -mr-2" ]
                ]
            ]
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewOriginToVarnishInteraction : Bool -> Dict String BodyColour -> Int -> List String -> List ( String, String ) -> String -> Html msg
viewOriginToVarnishInteraction showAllHeaders bodyToBodyColour_ statusCode extraKeysOfHeadersToShow headers body =
    div
        [ class "grid grid-cols-8" ]
        [ div [] []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 border-b-2 border-b-gray-700 p-2 pt-4 relative" ]
            [ viewStatusCode statusCode
            , ul
                [ class "list-disc list-inside mt-2 space-y-1" ]
                (viewResponseHeaders showAllHeaders extraKeysOfHeadersToShow headers)
            , div
                [ class "mt-2 font-mono" ]
                [ bodyBadge (Dict.get body bodyToBodyColour_) body ]
            , span
                [ class "absolute left-0 z-10" ]
                [ Icons.chevronLeft
                    [ Svg.Attributes.class "w-5 h-5 -ml-2" ]
                ]
            ]
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewVarnishToClientInteraction : Bool -> Dict String BodyColour -> Int -> List String -> List ( String, String ) -> String -> Html msg
viewVarnishToClientInteraction showAllHeaders bodyToBodyColour_ statusCode extraKeysOfHeadersToShow headers body =
    div
        [ class "grid grid-cols-8" ]
        [ div [] []
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300 border-b-2 border-b-gray-700 p-2 pt-4 relative" ]
            [ viewStatusCode statusCode
            , ul
                [ class "list-disc list-inside mt-2 space-y-1" ]
                (viewResponseHeaders showAllHeaders extraKeysOfHeadersToShow headers)
            , div
                [ class "mt-2 font-mono" ]
                [ bodyBadge (Dict.get body bodyToBodyColour_) body ]
            , span
                [ class "absolute left-0 z-10" ]
                [ Icons.chevronLeft
                    [ Svg.Attributes.class "w-5 h-5 -ml-2" ]
                ]
            ]
        , div
            [ class "col-span-3 border-l-2 border-l-gray-300" ]
            []
        , div
            [ class "border-l-2 border-l-gray-300" ]
            []
        ]


viewStatusCode : Int -> Html msg
viewStatusCode statusCode =
    let
        ( statusCodeAndMessage, explanation ) =
            case statusCode of
                200 ->
                    ( "200 OK", Just "The request has succeeded. A 200 response is cacheable by default." )

                304 ->
                    ( "304 Not Modified", Just "There is no need to retransmit the requested resources." )

                404 ->
                    ( "404 Not Found", Just "The server cannot find the requested resource." )

                _ ->
                    ( String.fromInt statusCode, Nothing )
    in
    case explanation of
        Just explanation_ ->
            details
                [ class "mt-1" ]
                [ summary
                    [ class "select-none" ]
                    [ span
                        [ class "ml-2" ]
                        [ text statusCodeAndMessage ]
                    ]
                , span
                    [ class "text-gray-500 font-sans" ]
                    [ text explanation_ ]
                ]

        Nothing ->
            div
                [ class "mt-1" ]
                [ text statusCodeAndMessage ]


viewRequestHeaders : Bool -> List String -> List ( String, String ) -> List (Html msg)
viewRequestHeaders showAllHeaders keysToShow headers =
    headers
        |> List.filter
            (\( key, _ ) ->
                showAllHeaders
                    || List.member (String.toLower key)
                        ([ "cache-control"
                         , "cookie"
                         , "if-match"
                         , "if-none-match"
                         , "if-modified-since"
                         , "if-unmodified-since"
                         ]
                            ++ keysToShow
                        )
            )
        |> List.map viewRequestHeader


viewRequestHeader : ( String, String ) -> Html msg
viewRequestHeader ( key, value ) =
    case String.toLower key of
        "accept-encoding" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Indicates the content encoding (usually a compression algorithm) that the client can understand." ]
                    ]
                ]

        "cache-control" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control#request_directives"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Ignored by Varnish in request headers." ]
                    ]
                ]

        "cookie" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cookie"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "By default, if the client sends a cookie header then Varnish will bypass the cache and go directly to the origin." ]
                    ]
                ]

        "if-match" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Makes the request "
                        , Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Match"
                            [ text "conditional" ]
                        , text "."
                        ]
                    ]
                ]

        "if-none-match" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Makes the request "
                        , Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match"
                            [ text "conditional" ]
                        , text "."
                        ]
                    ]
                ]

        "if-modified-since" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Makes the request "
                        , Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since"
                            [ text "conditional" ]
                        , text "."
                        ]
                    ]
                ]

        "if-unmodified-since" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Makes the request "
                        , Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since"
                            [ text "conditional" ]
                        , text "."
                        ]
                    ]
                ]

        "x-forwarded-for" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Identifies the "
                        , Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For"
                            [ text "originating IP address" ]
                        , text " of a client connecting through a proxy server."
                        ]
                    ]
                ]

        "x-varnish" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Contains the ID of the current request and (for a cache hit) the ID of the request that populated the cache."
                        ]
                    ]
                ]

        _ ->
            li
                [ class "font-mono" ]
                [ text <| key ++ ": " ++ value ]


viewCacheControlResponseDirective : String -> Html msg
viewCacheControlResponseDirective directive =
    let
        explanation =
            if String.startsWith "max-age=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                Just <| "The response remains fresh until " ++ Language.secondsFromString seconds ++ " after it is generated."

            else if directive == "no-store" then
                Just "Any caches of any kind (private or shared) should not store this response."

            else if directive == "private" then
                Just "The response can be stored only in a private cache (e.g. local caches in browsers)."

            else if String.startsWith "stale-while-revalidate=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                Just <| "The cache is allowed to reuse a stale response for " ++ Language.secondsFromString seconds ++ " while it revalidates it to a cache in the background."

            else if String.startsWith "s-maxage=" directive then
                let
                    seconds =
                        directive
                            |> String.split "="
                            |> List.tail
                            |> Maybe.map (String.join "=")
                            |> Maybe.withDefault ""
                in
                Just <|
                    "The response remains fresh for "
                        ++ Language.secondsFromString seconds
                        ++ " in a shared cache."

            else
                Nothing
    in
    case explanation of
        Just explanation_ ->
            details
                [ class "mt-1 ml-5" ]
                [ summary
                    [ class "select-none" ]
                    [ span
                        [ class "ml-2" ]
                        [ text directive ]
                    ]
                , span
                    [ class "text-gray-500 font-sans" ]
                    [ text explanation_ ]
                ]

        Nothing ->
            div [ class "mt-1 ml-5" ] [ text directive ]


viewResponseHeaders : Bool -> List String -> List ( String, String ) -> List (Html msg)
viewResponseHeaders showAllHeaders keysToShow headers =
    headers
        |> List.filter
            (\( key, _ ) ->
                showAllHeaders
                    || List.member (String.toLower key)
                        ([ "age"
                         , "cache-control"
                         , "content-encoding"
                         , "etag"
                         , "last-modified"
                         , "set-cookie"
                         , "vary"
                         ]
                            ++ keysToShow
                        )
            )
        |> List.map viewResponseHeader


viewResponseHeader : ( String, String ) -> Html msg
viewResponseHeader ( key, value ) =
    case String.toLower key of
        "age" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Age"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "The object has been in the cache for "
                        , text <| Language.secondsFromString value
                        , text "."
                        ]
                    ]
                ]

        "cache-control" ->
            li
                [ class "font-mono" ]
                (Extras.Html.externalLink
                    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control#response_directives"
                    [ text key ]
                    :: (value
                            |> String.split ","
                            |> List.map String.trim
                            |> List.map viewCacheControlResponseDirective
                       )
                )

        "content-encoding" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "The encodings that have been applied to the representation (message payload), in order, are "
                        , text value
                        , text "."
                        ]
                    ]
                ]

        "etag" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text <| "Indicates that "
                        , span
                            [ class "font-mono" ]
                            [ text value ]
                        , text " is an identifier for the current resource at this URL."
                        ]
                    ]
                ]

        "last-modified" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Last-Modified"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "The origin server believes the resource was last modified on "
                        , text value
                        , text "."
                        ]
                    ]
                ]

        "set-cookie" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "By default, Varnish will not cache an object coming from the origin with a Set-Cookie header present." ]
                    ]
                ]

        "vary" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Vary"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "The parts of the request message aside from the method and URL that influenced the content of the response it occurs in are: "
                        , text value
                        , text "."
                        ]
                    ]
                ]

        "x-cache" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ Extras.Html.externalLink
                            "https://www.varnish-software.com/developers/tutorials/logging-cache-hits-misses-varnish/"
                            [ span
                                [ class "ml-2" ]
                                [ text key ]
                            ]
                        , text <| ": " ++ value
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "(By convention) logs cache hits and misses."
                        ]
                    ]
                ]

        "x-served-by" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Identifies which point of presence the response came from."
                        ]
                    ]
                ]

        "x-varnish" ->
            li
                [ class "font-mono inline" ]
                [ details
                    [ class "mt-1" ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text <| key ++ ": " ++ value ]
                        ]
                    , span
                        [ class "text-gray-500 font-sans" ]
                        [ text "Contains the ID of the current request and (for a cache hit) the ID of the request that populated the cache."
                        ]
                    ]
                ]

        _ ->
            li
                [ class "font-mono" ]
                [ text <| key ++ ": " ++ value ]
