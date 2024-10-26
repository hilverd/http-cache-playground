module ScenarioForm exposing
    ( ClientAction(..)
    , Header
    , OriginHeader(..)
    , ScenarioForm
    , addGetRequestHeader
    , addMakeGetRequest
    , addMakePurgeRequest
    , addOriginCacheControlHeader
    , addOriginCustomHeader
    , addSleepForTwoSeconds
    , changeClientAction
    , changeCustomCacheControlDirectives
    , changeMaxAge
    , changeSMaxAge
    , changeStaleWhileRevalidate
    , clientActionHasGetRequestHeaderWithKey
    , clientActions
    , deleteClientAction
    , deleteGetRequestHeader
    , deleteOriginHeader
    , empty
    , exampleLinksByTitle
    , exerciseAnswers
    , exerciseTitle
    , fromUrl
    , hasCustomOriginHeaderWithKey
    , hasOriginCacheControlHeader
    , hasTenClientActions
    , indexOfLastCustomHeaderInGetRequest
    , isEmpty
    , originHeaders
    , originHeadersAsPairs
    , originReturn304ForConditionalRequests
    , originWait2SecondsBeforeResponding
    , selectExerciseAnswer
    , someExerciseAnswerIsSelected
    , toRelativeUrl
    , toScenario
    , toggleNoStore
    , toggleOriginReturn304ForConditionalRequests
    , toggleOriginWait2SecondsBeforeResponding
    , togglePrivate
    , updateGetRequestHeaderKey
    , updateGetRequestHeaderValue
    , updateOriginCustomHeaderKey
    , updateOriginCustomHeaderValue
    )

import Array exposing (Array)
import Data.CacheControlResponseDirectives as CacheControlResponseDirectives exposing (CacheControlResponseDirectives)
import Dict exposing (Dict)
import Extras.Array
import Html exposing (Html, text)
import QueryParameters exposing (QueryParameters)
import Scenario exposing (Action(..), Scenario)
import Url exposing (Url)


type alias ExerciseAnswer =
    { answer : String
    , selected : Bool
    , correct : Bool
    }


type ScenarioForm
    = ScenarioForm
        { clientActions : Array ClientAction
        , originWait2SecondsBeforeResponding : Bool
        , originHeaders : Array OriginHeader
        , originReturn304ForConditionalRequests : Bool
        , exerciseTitle : Maybe String
        , exerciseAnswers : Maybe (Array ExerciseAnswer)
        }


type OriginHeader
    = CacheControl CacheControlResponseDirectives
    | Custom Header


type alias Header =
    { key : String
    , value : String
    }


type ClientAction
    = MakeGetRequest (Array Header)
    | MakePurgeRequest
    | SleepForOneSecond
    | SleepForTwoSeconds
    | SleepForThreeSeconds
    | SleepForFiveSeconds
    | SleepForEightSeconds


create : List ClientAction -> Bool -> List OriginHeader -> Bool -> Maybe String -> Maybe (Array ExerciseAnswer) -> ScenarioForm
create clientActions_ originWait2SecondsBeforeResponding_ originHeaders_ originReturn304ForConditionalRequests_ exerciseTitle_ exerciseAnswers_ =
    ScenarioForm
        { clientActions = Array.fromList clientActions_
        , originWait2SecondsBeforeResponding = originWait2SecondsBeforeResponding_
        , originHeaders = Array.fromList originHeaders_
        , originReturn304ForConditionalRequests = originReturn304ForConditionalRequests_
        , exerciseTitle = exerciseTitle_
        , exerciseAnswers = exerciseAnswers_
        }


fromQueryParameters : QueryParameters -> ScenarioForm
fromQueryParameters queryParameters =
    let
        clientMakeGetRequestHeaderKeys : List ( Int, String )
        clientMakeGetRequestHeaderKeys =
            QueryParameters.clientMakeGetRequestHeaderKeys queryParameters

        clientMakeGetRequestHeaderValues : List ( Int, String )
        clientMakeGetRequestHeaderValues =
            QueryParameters.clientMakeGetRequestHeaderValues queryParameters
    in
    create
        (queryParameters
            |> QueryParameters.clientActionsWithoutDetails
            |> List.indexedMap
                (\stepIndex clientActionWithoutDetails ->
                    case clientActionWithoutDetails of
                        QueryParameters.MakeGetRequest ->
                            let
                                headerKeys : List String
                                headerKeys =
                                    clientMakeGetRequestHeaderKeys
                                        |> List.filter (Tuple.first >> (==) stepIndex)
                                        |> List.map Tuple.second

                                headerValues : List String
                                headerValues =
                                    clientMakeGetRequestHeaderValues
                                        |> List.filter (Tuple.first >> (==) stepIndex)
                                        |> List.map Tuple.second

                                headers : Array Header
                                headers =
                                    List.map2 Header headerKeys headerValues
                                        |> Array.fromList
                            in
                            MakeGetRequest headers

                        QueryParameters.MakePurgeRequest ->
                            MakePurgeRequest

                        QueryParameters.SleepForOneSecond ->
                            SleepForOneSecond

                        QueryParameters.SleepForTwoSeconds ->
                            SleepForTwoSeconds

                        QueryParameters.SleepForThreeSeconds ->
                            SleepForThreeSeconds

                        QueryParameters.SleepForFiveSeconds ->
                            SleepForFiveSeconds

                        QueryParameters.SleepForEightSeconds ->
                            SleepForEightSeconds
                )
        )
        (QueryParameters.originWait2SecondsBeforeResponding queryParameters)
        (let
            headerKeys : List String
            headerKeys =
                QueryParameters.originHeaderKeys queryParameters

            headerValues : List String
            headerValues =
                QueryParameters.originHeaderValues queryParameters

            headers : List OriginHeader
            headers =
                List.map2
                    (\key value ->
                        if String.toLower key == "cache-control" then
                            CacheControl <| CacheControlResponseDirectives.fromString value

                        else
                            Custom <| Header key value
                    )
                    headerKeys
                    headerValues
         in
         headers
        )
        (QueryParameters.originReturn304ForConditionalRequests queryParameters)
        Nothing
        Nothing


fromUrl : Url -> ScenarioForm
fromUrl url =
    url
        |> QueryParameters.exerciseIdFromUrl
        |> Maybe.andThen (\exerciseId -> Dict.get exerciseId exercisesById)
        |> Maybe.withDefault
            (url
                |> QueryParameters.fromUrl
                |> fromQueryParameters
            )


toQueryParameters : ScenarioForm -> QueryParameters
toQueryParameters (ScenarioForm form) =
    QueryParameters.create
        (form.clientActions
            |> Array.toList
            |> List.map
                (\clientAction ->
                    case clientAction of
                        MakeGetRequest _ ->
                            QueryParameters.MakeGetRequest

                        MakePurgeRequest ->
                            QueryParameters.MakePurgeRequest

                        SleepForOneSecond ->
                            QueryParameters.SleepForOneSecond

                        SleepForTwoSeconds ->
                            QueryParameters.SleepForTwoSeconds

                        SleepForThreeSeconds ->
                            QueryParameters.SleepForThreeSeconds

                        SleepForFiveSeconds ->
                            QueryParameters.SleepForFiveSeconds

                        SleepForEightSeconds ->
                            QueryParameters.SleepForEightSeconds
                )
        )
        (form.clientActions
            |> Array.toList
            |> List.indexedMap Tuple.pair
            |> List.filterMap
                (\( stepIndex, clientAction ) ->
                    case clientAction of
                        MakeGetRequest headers ->
                            headers |> Array.toList |> List.map (.key >> Tuple.pair stepIndex) |> Just

                        _ ->
                            Nothing
                )
            |> List.concat
        )
        (form.clientActions
            |> Array.toList
            |> List.indexedMap Tuple.pair
            |> List.filterMap
                (\( stepIndex, clientAction ) ->
                    case clientAction of
                        MakeGetRequest headers ->
                            headers |> Array.toList |> List.map (.value >> Tuple.pair stepIndex) |> Just

                        _ ->
                            Nothing
                )
            |> List.concat
        )
        form.originWait2SecondsBeforeResponding
        (form.originHeaders
            |> Array.toList
            |> List.filterMap
                (\originHeader ->
                    case originHeader of
                        Custom header ->
                            Just header.key

                        CacheControl _ ->
                            Just "Cache-Control"
                )
        )
        (form.originHeaders
            |> Array.toList
            |> List.filterMap
                (\originHeader ->
                    case originHeader of
                        Custom header ->
                            Just header.value

                        CacheControl cacheControlResponseDirectives ->
                            cacheControlResponseDirectives
                                |> CacheControlResponseDirectives.toString False
                                |> Just
                )
        )
        form.originReturn304ForConditionalRequests


toRelativeUrl : ScenarioForm -> String
toRelativeUrl scenarioForm =
    scenarioForm
        |> toQueryParameters
        |> QueryParameters.toRelativeUrl
        |> (\urlString ->
                if urlString == "" then
                    "?"

                else
                    urlString
           )


empty : ScenarioForm
empty =
    create [] False [] False Nothing Nothing


isEmpty : ScenarioForm -> Bool
isEmpty form =
    form == empty


hasTenClientActions : ScenarioForm -> Bool
hasTenClientActions (ScenarioForm form) =
    form.clientActions
        |> Array.length
        |> (==) 10


exerciseTitle : ScenarioForm -> Maybe String
exerciseTitle (ScenarioForm form) =
    form.exerciseTitle


exerciseAnswers : ScenarioForm -> Maybe (Array ExerciseAnswer)
exerciseAnswers (ScenarioForm form) =
    form.exerciseAnswers


clientActions : ScenarioForm -> Array ClientAction
clientActions (ScenarioForm form) =
    form.clientActions


indexOfLastCustomHeaderInGetRequest : ScenarioForm -> Maybe Int
indexOfLastCustomHeaderInGetRequest (ScenarioForm form) =
    form.clientActions
        |> Array.toList
        |> List.filterMap
            (\clientAction ->
                case clientAction of
                    MakeGetRequest headers ->
                        Just (Array.length headers - 1)

                    _ ->
                        Nothing
            )
        |> List.reverse
        |> List.head


originWait2SecondsBeforeResponding : ScenarioForm -> Bool
originWait2SecondsBeforeResponding (ScenarioForm form) =
    form.originWait2SecondsBeforeResponding


toggleOriginWait2SecondsBeforeResponding : ScenarioForm -> ScenarioForm
toggleOriginWait2SecondsBeforeResponding (ScenarioForm form) =
    ScenarioForm
        { form
            | originWait2SecondsBeforeResponding = not form.originWait2SecondsBeforeResponding
        }


originReturn304ForConditionalRequests : ScenarioForm -> Bool
originReturn304ForConditionalRequests (ScenarioForm form) =
    form.originReturn304ForConditionalRequests


toggleOriginReturn304ForConditionalRequests : ScenarioForm -> ScenarioForm
toggleOriginReturn304ForConditionalRequests (ScenarioForm form) =
    ScenarioForm
        { form
            | originReturn304ForConditionalRequests = not form.originReturn304ForConditionalRequests
        }


originHeadersAsPairs : ScenarioForm -> List ( String, String )
originHeadersAsPairs (ScenarioForm form) =
    form.originHeaders
        |> Array.toList
        |> List.map
            (\originHeader ->
                case originHeader of
                    CacheControl cacheControl ->
                        ( "Cache-Control", CacheControlResponseDirectives.toString False cacheControl )

                    Custom { key, value } ->
                        ( key, value )
            )


originHeaders : ScenarioForm -> List OriginHeader
originHeaders (ScenarioForm form) =
    Array.toList form.originHeaders


hasOriginCacheControlHeader : ScenarioForm -> Bool
hasOriginCacheControlHeader (ScenarioForm form) =
    form.originHeaders
        |> Array.toList
        |> List.any
            (\originHeader ->
                case originHeader of
                    CacheControl _ ->
                        True

                    _ ->
                        False
            )


hasCustomOriginHeaderWithKey : String -> ScenarioForm -> Bool
hasCustomOriginHeaderWithKey key (ScenarioForm form) =
    form.originHeaders
        |> Array.toList
        |> List.any
            (\originHeader ->
                case originHeader of
                    CacheControl _ ->
                        False

                    Custom header ->
                        header.key == key
            )


addOriginCustomHeader : String -> String -> ScenarioForm -> ScenarioForm
addOriginCustomHeader key value (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders = Array.push (Custom { key = key, value = value }) form.originHeaders
        }


deleteOriginHeader : Int -> ScenarioForm -> ScenarioForm
deleteOriginHeader index (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders = Extras.Array.delete index form.originHeaders
        }


updateOriginCustomHeaderKey : Int -> String -> ScenarioForm -> ScenarioForm
updateOriginCustomHeaderKey index key (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            Custom customHeader ->
                                Custom { customHeader | key = key }

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


updateOriginCustomHeaderValue : Int -> String -> ScenarioForm -> ScenarioForm
updateOriginCustomHeaderValue index value (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            Custom customHeader ->
                                Custom { customHeader | value = value }

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


addOriginCacheControlHeader : ScenarioForm -> ScenarioForm
addOriginCacheControlHeader (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders = Array.push (CacheControl CacheControlResponseDirectives.empty) form.originHeaders
        }


changeMaxAge : Int -> Maybe Int -> ScenarioForm -> ScenarioForm
changeMaxAge index maxAge (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <| CacheControlResponseDirectives.updateMaxAge maxAge cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


changeSMaxAge : Int -> Maybe Int -> ScenarioForm -> ScenarioForm
changeSMaxAge index sMaxAge (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <| CacheControlResponseDirectives.updateSMaxAge sMaxAge cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


toggleNoStore : Int -> ScenarioForm -> ScenarioForm
toggleNoStore index (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <|
                                    CacheControlResponseDirectives.updateNoStore
                                        (not <| CacheControlResponseDirectives.noStore cacheControl)
                                        cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


togglePrivate : Int -> ScenarioForm -> ScenarioForm
togglePrivate index (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <|
                                    CacheControlResponseDirectives.updatePrivate
                                        (not <| CacheControlResponseDirectives.private cacheControl)
                                        cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


changeStaleWhileRevalidate : Int -> Maybe Int -> ScenarioForm -> ScenarioForm
changeStaleWhileRevalidate index staleWhileRevalidate (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <| CacheControlResponseDirectives.updateStaleWhileRevalidate staleWhileRevalidate cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


changeCustomCacheControlDirectives : Int -> String -> ScenarioForm -> ScenarioForm
changeCustomCacheControlDirectives index value (ScenarioForm form) =
    ScenarioForm
        { form
            | originHeaders =
                Extras.Array.update
                    (\header ->
                        case header of
                            CacheControl cacheControl ->
                                CacheControl <| CacheControlResponseDirectives.updateCustom value cacheControl

                            _ ->
                                header
                    )
                    index
                    form.originHeaders
        }


addMakeGetRequest : ScenarioForm -> ScenarioForm
addMakeGetRequest (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions = Array.push (MakeGetRequest Array.empty) form.clientActions
        }


addMakePurgeRequest : ScenarioForm -> ScenarioForm
addMakePurgeRequest (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions = Array.push MakePurgeRequest form.clientActions
        }


addGetRequestHeader : Int -> String -> String -> ScenarioForm -> ScenarioForm
addGetRequestHeader index key value (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions =
                Extras.Array.update
                    (\action ->
                        case action of
                            MakeGetRequest headers ->
                                MakeGetRequest <| Array.push { key = key, value = value } headers

                            _ ->
                                action
                    )
                    index
                    form.clientActions
        }


clientActionHasGetRequestHeaderWithKey : String -> ClientAction -> Bool
clientActionHasGetRequestHeaderWithKey key clientAction =
    case clientAction of
        MakeGetRequest headers ->
            headers
                |> Array.toList
                |> List.any (\header -> header.key == key)

        _ ->
            False


updateGetRequestHeaderKey : Int -> Int -> String -> ScenarioForm -> ScenarioForm
updateGetRequestHeaderKey index headerIndex key (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions =
                Extras.Array.update
                    (\action ->
                        case action of
                            MakeGetRequest headers ->
                                MakeGetRequest <|
                                    Extras.Array.update
                                        (\header -> { header | key = key })
                                        headerIndex
                                        headers

                            _ ->
                                action
                    )
                    index
                    form.clientActions
        }


updateGetRequestHeaderValue : Int -> Int -> String -> ScenarioForm -> ScenarioForm
updateGetRequestHeaderValue index headerIndex value (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions =
                Extras.Array.update
                    (\action ->
                        case action of
                            MakeGetRequest headers ->
                                MakeGetRequest <|
                                    Extras.Array.update
                                        (\header -> { header | value = value })
                                        headerIndex
                                        headers

                            _ ->
                                action
                    )
                    index
                    form.clientActions
        }


deleteGetRequestHeader : Int -> Int -> ScenarioForm -> ScenarioForm
deleteGetRequestHeader index headerIndex (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions =
                Extras.Array.update
                    (\action ->
                        case action of
                            MakeGetRequest headers ->
                                MakeGetRequest <| Extras.Array.delete headerIndex headers

                            _ ->
                                action
                    )
                    index
                    form.clientActions
        }


deleteClientAction : Int -> ScenarioForm -> ScenarioForm
deleteClientAction index (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions = Extras.Array.delete index form.clientActions
        }


addSleepForTwoSeconds : ScenarioForm -> ScenarioForm
addSleepForTwoSeconds (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions = Array.push SleepForTwoSeconds form.clientActions
        }


changeClientAction : Int -> ClientAction -> ScenarioForm -> ScenarioForm
changeClientAction index newClientAction (ScenarioForm form) =
    ScenarioForm
        { form
            | clientActions = Extras.Array.update (always newClientAction) index form.clientActions
        }


toScenario : ScenarioForm -> String -> Scenario
toScenario ((ScenarioForm form) as scenarioForm) id =
    let
        path : String
        path =
            "/ids/" ++ id

        desiredResponseHeaders : List ( String, String )
        desiredResponseHeaders =
            scenarioForm
                |> originHeadersAsPairs
                |> List.filter (\( key, value ) -> key /= "" && value /= "")
    in
    form.clientActions
        |> Array.toList
        |> List.map
            (\clientAction ->
                case clientAction of
                    MakeGetRequest headers ->
                        Scenario.MakeGetRequest
                            { path = path
                            , headers =
                                headers
                                    |> Array.toList
                                    |> List.map (\{ key, value } -> ( key, value ))
                                    |> List.filter (\( key, value ) -> key /= "" && value /= "")
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = form.originWait2SecondsBeforeResponding
                            , auto304 = form.originReturn304ForConditionalRequests
                            }

                    MakePurgeRequest ->
                        Scenario.MakePurgeRequest
                            { path = path
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = form.originWait2SecondsBeforeResponding
                            , auto304 = form.originReturn304ForConditionalRequests
                            }

                    SleepForOneSecond ->
                        Scenario.SleepForSeconds 1

                    SleepForTwoSeconds ->
                        Scenario.SleepForSeconds 2

                    SleepForThreeSeconds ->
                        Scenario.SleepForSeconds 3

                    SleepForFiveSeconds ->
                        Scenario.SleepForSeconds 5

                    SleepForEightSeconds ->
                        Scenario.SleepForSeconds 8
            )
        |> List.indexedMap Tuple.pair
        |> Scenario.create id


selectExerciseAnswer : Int -> ScenarioForm -> ScenarioForm
selectExerciseAnswer index (ScenarioForm form) =
    let
        updatedExerciseAnswers =
            form.exerciseAnswers
                |> Maybe.map
                    (\answers ->
                        answers
                            |> Array.indexedMap
                                (\index_ answer ->
                                    { answer | selected = index_ == index }
                                )
                    )
    in
    ScenarioForm
        { form | exerciseAnswers = updatedExerciseAnswers }


someExerciseAnswerIsSelected : ScenarioForm -> Bool
someExerciseAnswerIsSelected (ScenarioForm form) =
    form.exerciseAnswers
        |> Maybe.map
            (\answers ->
                answers
                    |> Array.toList
                    |> List.any (\answer -> answer.selected)
            )
        |> Maybe.withDefault False



-- EXERCISES


exerciseStaleWhileRevalidate1 : ScenarioForm
exerciseStaleWhileRevalidate1 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        True
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 1)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        ]
        False
        (Just "stale-while-revalidate (1)")
        ([ { answer = "Varnish returns a response with the same body as before", selected = False, correct = True }
         , { answer = "Varnish returns a different response body", selected = False, correct = False }
         ]
            |> Array.fromList
            |> Just
        )


exerciseStaleWhileRevalidate2 : ScenarioForm
exerciseStaleWhileRevalidate2 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 1)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 1)
            |> CacheControl
        ]
        False
        (Just "stale-while-revalidate (2)")
        ([ { answer = "Varnish returns a response with the same body as before", selected = False, correct = False }
         , { answer = "Varnish returns a different response body", selected = False, correct = True }
         ]
            |> Array.fromList
            |> Just
        )


exerciseAge1 : ScenarioForm
exerciseAge1 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "4" }
        ]
        False
        (Just "The Age header (1)")
        ([ { answer = "Varnish returns a 200 with an age of 4", selected = False, correct = True }
         , { answer = "Varnish returns a 200 with an age of 0", selected = False, correct = False }
         ]
            |> Array.fromList
            |> Just
        )


exerciseAge2 : ScenarioForm
exerciseAge2 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "4" }
        ]
        False
        (Just "The Age header (2)")
        ([ { answer = "Varnish returns a response with the same body as before, with an age of 1", selected = False, correct = False }
         , { answer = "Varnish returns a response with the same body as before, with an age of 6", selected = False, correct = True }
         , { answer = "Varnish returns a different response body", selected = False, correct = False }
         ]
            |> Array.fromList
            |> Just
        )


exerciseAge3 : ScenarioForm
exerciseAge3 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "5" }
        ]
        False
        (Just "The Age header (3)")
        ([ { answer = "Varnish returns a response with the same body as before", selected = False, correct = False }
         , { answer = "Varnish returns a different response body", selected = False, correct = True }
         ]
            |> Array.fromList
            |> Just
        )


exerciseConditionalRequestsWorkflow1 : ScenarioForm
exerciseConditionalRequestsWorkflow1 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        True
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 2)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        True
        (Just "Workflow for conditional requests (1)")
        ([ { answer = "Varnish returns a 200 with an age of 0 and the same body as before", selected = False, correct = False }
         , { answer = "Varnish returns a 200 with an age of 3 and the same body as before", selected = False, correct = True }
         , { answer = "Varnish returns a 304 with an empty body", selected = False, correct = False }
         ]
            |> Array.fromList
            |> Just
        )


exerciseConditionalRequestsWorkflow2 : ScenarioForm
exerciseConditionalRequestsWorkflow2 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 2)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        True
        (Just "Workflow for conditional requests (2)")
        ([ { answer = "Varnish returns a 200 and also makes a revalidation request", selected = False, correct = False }
         , { answer = "Varnish returns a 200 and does not make a revalidation request", selected = False, correct = True }
         ]
            |> Array.fromList
            |> Just
        )


exerciseConditionalRequestsWorkflow3 : ScenarioForm
exerciseConditionalRequestsWorkflow3 =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([ { key = "If-None-Match", value = "\"some-etag\"" } ] |> Array.fromList)
        ]
        False
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        False
        (Just "Workflow for conditional requests (3)")
        ([ { answer = "Varnish returns a 200 and also makes a revalidation request", selected = False, correct = False }
         , { answer = "Varnish returns a 200 and does not make a revalidation request", selected = False, correct = False }
         , { answer = "Varnish returns a 304 and also makes a revalidation request", selected = False, correct = False }
         , { answer = "Varnish returns a 304 and does not make a revalidation request", selected = False, correct = True }
         ]
            |> Array.fromList
            |> Just
        )


exercisesById : Dict String ScenarioForm
exercisesById =
    Dict.fromList
        [ ( "stale-while-revalidate-1", exerciseStaleWhileRevalidate1 )
        , ( "stale-while-revalidate-2", exerciseStaleWhileRevalidate2 )
        , ( "age-1", exerciseAge1 )
        , ( "age-2", exerciseAge2 )
        , ( "age-3", exerciseAge3 )
        , ( "conditional-requests-workflow-1", exerciseConditionalRequestsWorkflow1 )
        , ( "conditional-requests-workflow-2", exerciseConditionalRequestsWorkflow2 )
        , ( "conditional-requests-workflow-3", exerciseConditionalRequestsWorkflow3 )
        ]



-- EXAMPLES


example200CacheableByDefault : ScenarioForm
example200CacheableByDefault =
    create
        [ MakeGetRequest Array.empty
        , SleepForOneSecond
        , MakeGetRequest Array.empty
        ]
        False
        []
        False
        Nothing
        Nothing


eaxmpleStaleResponsesAreRevalidatedByDefault : ScenarioForm
eaxmpleStaleResponsesAreRevalidatedByDefault =
    create
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        True
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateMaxAge (Just 1)
            |> CacheControl
        ]
        False
        Nothing
        Nothing


exampleLinksByTitle : List ( Html msg, String )
exampleLinksByTitle =
    [ ( text "200 OK responses are cacheable by default", toRelativeUrl example200CacheableByDefault )
    , ( text "Stale responses are revalidated by default", toRelativeUrl eaxmpleStaleResponsesAreRevalidatedByDefault )
    ]
