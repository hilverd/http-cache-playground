module QueryParameters exposing (ClientActionWithoutDetails(..), QueryParameters, clientActionsWithoutDetails, clientMakeGetRequestHeaderKeys, clientMakeGetRequestHeaderValues, create, exerciseIdFromUrl, fromUrl, originHeaderKeys, originHeaderValues, originReturn304ForConditionalRequests, originWait2SecondsBeforeResponding, toRelativeUrl)

import Dict
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query


type ClientActionWithoutDetails
    = MakeGetRequest
    | SleepForOneSecond
    | SleepForTwoSeconds
    | SleepForThreeSeconds
    | SleepForFiveSeconds
    | SleepForEightSeconds


type QueryParameters
    = QueryParameters
        { clientActionsWithoutDetails : List ClientActionWithoutDetails
        , clientMakeGetRequestHeaderKeys : List ( Int, String )
        , clientMakeGetRequestHeaderValues : List ( Int, String )
        , originWait2SecondsBeforeResponding : Bool
        , originReturn304ForConditionalRequests : Bool
        , originHeaderKeys : List String
        , originHeaderValues : List String
        }


create :
    List ClientActionWithoutDetails
    -> List ( Int, String )
    -> List ( Int, String )
    -> Bool
    -> List String
    -> List String
    -> Bool
    -> QueryParameters
create clientActionsWithoutDetails_ clientMakeGetRequestHeaderKeys_ clientMakeGetRequestHeaderValues_ originWait2SecondsBeforeResponding_ originHeaderKeys_ originHeaderValues_ originReturn304ForConditionalRequests_ =
    QueryParameters
        { clientActionsWithoutDetails = clientActionsWithoutDetails_
        , clientMakeGetRequestHeaderKeys = clientMakeGetRequestHeaderKeys_
        , clientMakeGetRequestHeaderValues = clientMakeGetRequestHeaderValues_
        , originWait2SecondsBeforeResponding = originWait2SecondsBeforeResponding_
        , originHeaderKeys = originHeaderKeys_
        , originHeaderValues = originHeaderValues_
        , originReturn304ForConditionalRequests = originReturn304ForConditionalRequests_
        }


default : QueryParameters
default =
    create [] [] [] False [] [] False


clientActionsWithoutDetails : QueryParameters -> List ClientActionWithoutDetails
clientActionsWithoutDetails (QueryParameters queryParameters) =
    queryParameters.clientActionsWithoutDetails


clientMakeGetRequestHeaderKeys : QueryParameters -> List ( Int, String )
clientMakeGetRequestHeaderKeys (QueryParameters queryParameters) =
    queryParameters.clientMakeGetRequestHeaderKeys


clientMakeGetRequestHeaderValues : QueryParameters -> List ( Int, String )
clientMakeGetRequestHeaderValues (QueryParameters queryParameters) =
    queryParameters.clientMakeGetRequestHeaderValues


clientActionsWithoutDetailsFromQuery : Int -> Url.Parser.Query.Parser (Maybe ClientActionWithoutDetails)
clientActionsWithoutDetailsFromQuery stepIndex =
    Url.Parser.Query.custom ("a" ++ String.fromInt stepIndex) <|
        \strings ->
            case strings of
                [ "get" ] ->
                    Just MakeGetRequest

                [ "s-1" ] ->
                    Just SleepForOneSecond

                [ "s-2" ] ->
                    Just SleepForTwoSeconds

                [ "s-3" ] ->
                    Just SleepForThreeSeconds

                [ "s-5" ] ->
                    Just SleepForFiveSeconds

                [ "s-8" ] ->
                    Just SleepForEightSeconds

                _ ->
                    Nothing


clientActionWithoutDetailsToQuery : Int -> ClientActionWithoutDetails -> Url.Builder.QueryParameter
clientActionWithoutDetailsToQuery stepIndex clientActionWithoutDetails_ =
    (case clientActionWithoutDetails_ of
        MakeGetRequest ->
            "get"

        SleepForOneSecond ->
            "s-1"

        SleepForTwoSeconds ->
            "s-2"

        SleepForThreeSeconds ->
            "s-3"

        SleepForFiveSeconds ->
            "s-5"

        SleepForEightSeconds ->
            "s-8"
    )
        |> Url.Builder.string ("a" ++ String.fromInt stepIndex)


clientMakeGetRequestHeaderKeyToQuery : Int -> Int -> String -> Url.Builder.QueryParameter
clientMakeGetRequestHeaderKeyToQuery stepIndex headerIndex =
    Url.Builder.string ("a" ++ String.fromInt stepIndex ++ "h" ++ String.fromInt headerIndex ++ "k")


clientMakeGetRequestHeaderValueToQuery : Int -> Int -> String -> Url.Builder.QueryParameter
clientMakeGetRequestHeaderValueToQuery stepIndex headerIndex =
    Url.Builder.string ("a" ++ String.fromInt stepIndex ++ "h" ++ String.fromInt headerIndex ++ "v")


originWait2SecondsBeforeResponding : QueryParameters -> Bool
originWait2SecondsBeforeResponding (QueryParameters queryParameters) =
    queryParameters.originWait2SecondsBeforeResponding


originReturn304ForConditionalRequests : QueryParameters -> Bool
originReturn304ForConditionalRequests (QueryParameters queryParameters) =
    queryParameters.originReturn304ForConditionalRequests


originWait2SecondsBeforeRespondingFromQuery : Url.Parser.Query.Parser Bool
originWait2SecondsBeforeRespondingFromQuery =
    Url.Parser.Query.custom "wait" <|
        \maybeValue ->
            case maybeValue of
                [ "y" ] ->
                    True

                _ ->
                    False


originReturn304ForConditionalRequestsFromQuery : Url.Parser.Query.Parser Bool
originReturn304ForConditionalRequestsFromQuery =
    Url.Parser.Query.custom "auto-304" <|
        \maybeValue ->
            case maybeValue of
                [ "y" ] ->
                    True

                _ ->
                    False


originWait2SecondsBeforeRespondingToQuery : Bool -> Maybe Url.Builder.QueryParameter
originWait2SecondsBeforeRespondingToQuery originWait2SecondsBeforeResponding_ =
    (if originWait2SecondsBeforeResponding_ then
        Just "y"

     else
        Nothing
    )
        |> Maybe.map (Url.Builder.string "wait")


originReturn304ForConditionalRequestsToQuery : Bool -> Maybe Url.Builder.QueryParameter
originReturn304ForConditionalRequestsToQuery originReturn304ForConditionalRequests_ =
    (if originReturn304ForConditionalRequests_ then
        Just "y"

     else
        Nothing
    )
        |> Maybe.map (Url.Builder.string "auto-304")


clientActionsParser : Url.Parser.Query.Parser (List (Maybe ClientActionWithoutDetails))
clientActionsParser =
    List.range 1 9
        |> List.foldl
            (clientActionsWithoutDetailsFromQuery
                >> Url.Parser.Query.map2 (::)
            )
            (0
                |> clientActionsWithoutDetailsFromQuery
                |> Url.Parser.Query.map (\x -> [ x ])
            )


clientMakeGetRequestHeaderKeyFromQuery : Int -> Int -> Url.Parser.Query.Parser (Maybe ( Int, String ))
clientMakeGetRequestHeaderKeyFromQuery stepIndex index =
    Url.Parser.Query.string ("a" ++ String.fromInt stepIndex ++ "h" ++ String.fromInt index ++ "k")
        |> Url.Parser.Query.map (Maybe.map (Tuple.pair stepIndex))


clientMakeGetRequestHeaderValueFromQuery : Int -> Int -> Url.Parser.Query.Parser (Maybe ( Int, String ))
clientMakeGetRequestHeaderValueFromQuery stepIndex index =
    Url.Parser.Query.string ("a" ++ String.fromInt stepIndex ++ "h" ++ String.fromInt index ++ "v")
        |> Url.Parser.Query.map (Maybe.map (Tuple.pair stepIndex))


clientMakeGetRequestHeaderKeysParser : Url.Parser.Query.Parser (List (Maybe ( Int, String )))
clientMakeGetRequestHeaderKeysParser =
    let
        stepIndexes : List Int
        stepIndexes =
            List.range 0 9

        headerIndexes : List Int
        headerIndexes =
            List.range 0 9

        cartesianProduct : List ( Int, Int )
        cartesianProduct =
            stepIndexes
                |> List.foldr
                    (\stepIndex result ->
                        List.map (Tuple.pair stepIndex) headerIndexes
                            ++ result
                    )
                    []
    in
    cartesianProduct
        |> List.filter
            (\( stepIndex, headerIndex ) ->
                not (stepIndex == 0 && headerIndex == 0)
            )
        |> List.foldl
            ((\( stepIndex, index ) -> clientMakeGetRequestHeaderKeyFromQuery stepIndex index)
                >> Url.Parser.Query.map2 (::)
            )
            (( 0, 0 )
                |> (\( stepIndex, index ) -> clientMakeGetRequestHeaderKeyFromQuery stepIndex index)
                |> Url.Parser.Query.map (\x -> [ x ])
            )


clientMakeGetRequestHeaderValuesParser : Url.Parser.Query.Parser (List (Maybe ( Int, String )))
clientMakeGetRequestHeaderValuesParser =
    let
        stepIndexes : List Int
        stepIndexes =
            List.range 0 9

        headerIndexes : List Int
        headerIndexes =
            List.range 0 9

        cartesianProduct : List ( Int, Int )
        cartesianProduct =
            stepIndexes
                |> List.foldr
                    (\stepIndex result ->
                        List.map (Tuple.pair stepIndex) headerIndexes
                            ++ result
                    )
                    []
    in
    cartesianProduct
        |> List.filter
            (\( stepIndex, headerIndex ) ->
                not (stepIndex == 0 && headerIndex == 0)
            )
        |> List.foldl
            ((\( stepIndex, index ) -> clientMakeGetRequestHeaderValueFromQuery stepIndex index)
                >> Url.Parser.Query.map2 (::)
            )
            (( 0, 0 )
                |> (\( stepIndex, index ) -> clientMakeGetRequestHeaderValueFromQuery stepIndex index)
                |> Url.Parser.Query.map (\x -> [ x ])
            )


originHeaderKeys : QueryParameters -> List String
originHeaderKeys (QueryParameters queryParameters) =
    queryParameters.originHeaderKeys


originHeaderKeyFromQuery : Int -> Url.Parser.Query.Parser (Maybe String)
originHeaderKeyFromQuery index =
    Url.Parser.Query.string ("h" ++ String.fromInt index ++ "k")


originHeaderKeysParser : Url.Parser.Query.Parser (List (Maybe String))
originHeaderKeysParser =
    List.range 1 9
        |> List.foldl
            ((\index -> originHeaderKeyFromQuery index)
                >> Url.Parser.Query.map2 (::)
            )
            (0
                |> (\index -> originHeaderKeyFromQuery index)
                |> Url.Parser.Query.map (\x -> [ x ])
            )


originHeaderValues : QueryParameters -> List String
originHeaderValues (QueryParameters queryParameters) =
    queryParameters.originHeaderValues


originHeaderValueFromQuery : Int -> Url.Parser.Query.Parser (Maybe String)
originHeaderValueFromQuery index =
    Url.Parser.Query.string ("h" ++ String.fromInt index ++ "v")


originHeaderKeyToQuery : Int -> String -> Url.Builder.QueryParameter
originHeaderKeyToQuery headerIndex =
    Url.Builder.string ("h" ++ String.fromInt headerIndex ++ "k")


originHeaderValuesParser : Url.Parser.Query.Parser (List (Maybe String))
originHeaderValuesParser =
    List.range 1 9
        |> List.foldl
            ((\index -> originHeaderValueFromQuery index)
                >> Url.Parser.Query.map2 (::)
            )
            (0
                |> (\index -> originHeaderValueFromQuery index)
                |> Url.Parser.Query.map (\x -> [ x ])
            )


originHeaderValueToQuery : Int -> String -> Url.Builder.QueryParameter
originHeaderValueToQuery headerIndex =
    Url.Builder.string ("h" ++ String.fromInt headerIndex ++ "v")


query : Url.Parser.Query.Parser QueryParameters
query =
    Url.Parser.Query.map7 create
        (clientActionsParser |> Url.Parser.Query.map (List.filterMap identity >> List.reverse))
        (clientMakeGetRequestHeaderKeysParser
            |> Url.Parser.Query.map
                (List.filterMap identity >> List.reverse)
        )
        (clientMakeGetRequestHeaderValuesParser
            |> Url.Parser.Query.map
                (List.filterMap identity >> List.reverse)
        )
        originWait2SecondsBeforeRespondingFromQuery
        (originHeaderKeysParser
            |> Url.Parser.Query.map
                (List.filterMap identity >> List.reverse)
        )
        (originHeaderValuesParser
            |> Url.Parser.Query.map
                (List.filterMap identity >> List.reverse)
        )
        originReturn304ForConditionalRequestsFromQuery


parser : Url.Parser.Parser (QueryParameters -> a) a
parser =
    Url.Parser.query query


fromUrl : Url -> QueryParameters
fromUrl url =
    { url | path = "" }
        |> Url.Parser.parse parser
        |> Maybe.withDefault default


exerciseIdFromUrl : Url -> Maybe String
exerciseIdFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.query
                (Url.Parser.Query.custom "exercise-id" <|
                    List.head
                )
            )
        |> Maybe.withDefault Nothing


toRelativeUrl : QueryParameters -> String
toRelativeUrl (QueryParameters queryParameters) =
    let
        clientActionsWithoutDetails_ : List Url.Builder.QueryParameter
        clientActionsWithoutDetails_ =
            queryParameters.clientActionsWithoutDetails
                |> List.indexedMap clientActionWithoutDetailsToQuery

        clientMakeGetRequestHeaderKeysByStepIndex : List ( Int, List String )
        clientMakeGetRequestHeaderKeysByStepIndex =
            queryParameters.clientMakeGetRequestHeaderKeys
                |> List.foldl
                    (\( stepIndex, key ) result ->
                        Dict.update
                            stepIndex
                            (\currentKeys ->
                                case currentKeys of
                                    Just current ->
                                        Just (key :: current)

                                    Nothing ->
                                        Just [ key ]
                            )
                            result
                    )
                    Dict.empty
                |> Dict.map (always List.reverse)
                |> Dict.toList

        clientMakeGetRequestHeaderValuesByStepIndex : List ( Int, List String )
        clientMakeGetRequestHeaderValuesByStepIndex =
            queryParameters.clientMakeGetRequestHeaderValues
                |> List.foldl
                    (\( stepIndex, value ) result ->
                        Dict.update
                            stepIndex
                            (\currentValue ->
                                case currentValue of
                                    Just current ->
                                        Just (value :: current)

                                    Nothing ->
                                        Just [ value ]
                            )
                            result
                    )
                    Dict.empty
                |> Dict.map (always List.reverse)
                |> Dict.toList

        clientMakeGetRequestHeaderKeys_ : List Url.Builder.QueryParameter
        clientMakeGetRequestHeaderKeys_ =
            clientMakeGetRequestHeaderKeysByStepIndex
                |> List.map
                    (\( stepIndex, headerKeys ) ->
                        headerKeys
                            |> List.indexedMap
                                (\headerIndex headerKey ->
                                    ( stepIndex, headerIndex, headerKey )
                                )
                    )
                |> List.concat
                |> List.map
                    (\( stepIndex, headerIndex, key ) ->
                        clientMakeGetRequestHeaderKeyToQuery stepIndex headerIndex key
                    )

        clientMakeGetRequestHeaderValues_ : List Url.Builder.QueryParameter
        clientMakeGetRequestHeaderValues_ =
            clientMakeGetRequestHeaderValuesByStepIndex
                |> List.map
                    (\( stepIndex, headerValues ) ->
                        headerValues
                            |> List.indexedMap
                                (\headerIndex headerValue ->
                                    ( stepIndex, headerIndex, headerValue )
                                )
                    )
                |> List.concat
                |> List.map
                    (\( stepIndex, headerIndex, value ) ->
                        clientMakeGetRequestHeaderValueToQuery stepIndex headerIndex value
                    )

        originHeaderKeys_ : List Url.Builder.QueryParameter
        originHeaderKeys_ =
            queryParameters.originHeaderKeys
                |> List.indexedMap originHeaderKeyToQuery

        originHeaderValues_ : List Url.Builder.QueryParameter
        originHeaderValues_ =
            queryParameters.originHeaderValues
                |> List.indexedMap originHeaderValueToQuery
    in
    clientActionsWithoutDetails_
        ++ clientMakeGetRequestHeaderKeys_
        ++ clientMakeGetRequestHeaderValues_
        ++ ([ queryParameters.originWait2SecondsBeforeResponding
                |> originWait2SecondsBeforeRespondingToQuery
            ]
                |> List.filterMap identity
           )
        ++ originHeaderKeys_
        ++ originHeaderValues_
        ++ ([ queryParameters.originReturn304ForConditionalRequests
                |> originReturn304ForConditionalRequestsToQuery
            ]
                |> List.filterMap identity
           )
        |> Url.Builder.relative []
