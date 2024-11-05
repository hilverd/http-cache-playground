module ScenarioForm exposing
    ( ClientAction(..)
    , Header
    , Mode(..)
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
    , exampleInteractionsByRelativeUrl
    , exampleLinksByTitle
    , fromUrl
    , hasCustomOriginHeaderWithKey
    , hasOriginCacheControlHeader
    , hasTenClientActions
    , indexOfLastCustomHeaderInGetRequest
    , isEmpty
    , isExercise
    , mode
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
import Codec
import Data.CacheControlResponseDirectives as CacheControlResponseDirectives exposing (CacheControlResponseDirectives)
import Data.Scenario as Scenario exposing (Action(..), Scenario)
import Dict exposing (Dict)
import Extras.Array
import Html exposing (Html, text)
import Interactions exposing (Interactions)
import QueryParameters exposing (QueryParameters)
import Url exposing (Url)


type alias ExerciseAnswer =
    { answer : String
    , selected : Bool
    , correct : Bool
    }


type Mode
    = Normal
    | Example
        { title : String
        , interactionsJson : String
        }
    | Exercise
        { title : String
        , answers : Array ExerciseAnswer
        }


type ScenarioForm
    = ScenarioForm
        { clientActions : Array ClientAction
        , originWait2SecondsBeforeResponding : Bool
        , originHeaders : Array OriginHeader
        , originReturn304ForConditionalRequests : Bool
        , mode : Mode
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


create :
    { originWait2SecondsBeforeResponding_ : Bool
    , originReturn304ForConditionalRequests_ : Bool
    }
    -> List ClientAction
    -> List OriginHeader
    -> Mode
    -> ScenarioForm
create { originWait2SecondsBeforeResponding_, originReturn304ForConditionalRequests_ } clientActions_ originHeaders_ mode_ =
    ScenarioForm
        { clientActions = Array.fromList clientActions_
        , originWait2SecondsBeforeResponding = originWait2SecondsBeforeResponding_
        , originHeaders = Array.fromList originHeaders_
        , originReturn304ForConditionalRequests = originReturn304ForConditionalRequests_
        , mode = mode_
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
        { originWait2SecondsBeforeResponding_ = QueryParameters.originWait2SecondsBeforeResponding queryParameters
        , originReturn304ForConditionalRequests_ = QueryParameters.originReturn304ForConditionalRequests queryParameters
        }
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
        Normal


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
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        []
        []
        Normal


isEmpty : ScenarioForm -> Bool
isEmpty form =
    form == empty


hasTenClientActions : ScenarioForm -> Bool
hasTenClientActions (ScenarioForm form) =
    form.clientActions
        |> Array.length
        |> (==) 10


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


mode : ScenarioForm -> Mode
mode (ScenarioForm form) =
    form.mode


isExercise : ScenarioForm -> Bool
isExercise (ScenarioForm form) =
    case form.mode of
        Exercise _ ->
            True

        _ ->
            False


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
        mode0 =
            form.mode

        mode_ : Mode
        mode_ =
            case mode0 of
                Exercise ({ answers } as exercise) ->
                    Exercise
                        { exercise
                            | answers =
                                answers
                                    |> Array.indexedMap
                                        (\index_ answer ->
                                            { answer | selected = index_ == index }
                                        )
                        }

                _ ->
                    mode0
    in
    ScenarioForm
        { form | mode = mode_ }


someExerciseAnswerIsSelected : ScenarioForm -> Bool
someExerciseAnswerIsSelected (ScenarioForm form) =
    case form.mode of
        Exercise { answers } ->
            answers
                |> Array.toList
                |> List.any .selected

        _ ->
            False



-- EXERCISES


exerciseStaleWhileRevalidate1 : ScenarioForm
exerciseStaleWhileRevalidate1 =
    create
        { originWait2SecondsBeforeResponding_ = True
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 1)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        ]
        (Exercise
            { title = "stale-while-revalidate (1)"
            , answers =
                [ { answer = "Varnish returns a response with the same body as before", selected = False, correct = True }
                , { answer = "Varnish returns a different response body", selected = False, correct = False }
                ]
                    |> Array.fromList
            }
        )


exerciseStaleWhileRevalidate2 : ScenarioForm
exerciseStaleWhileRevalidate2 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 1)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 1)
            |> CacheControl
        ]
        (Exercise
            { title = "stale-while-revalidate (2)"
            , answers =
                [ { answer = "Varnish returns a response with the same body as before", selected = False, correct = False }
                , { answer = "Varnish returns a different response body", selected = False, correct = True }
                ]
                    |> Array.fromList
            }
        )


exerciseAge1 : ScenarioForm
exerciseAge1 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "4" }
        ]
        (Exercise
            { title = "The Age header (1)"
            , answers =
                [ { answer = "Varnish returns a 200 with an age of 4", selected = False, correct = True }
                , { answer = "Varnish returns a 200 with an age of 0", selected = False, correct = False }
                ]
                    |> Array.fromList
            }
        )


exerciseAge2 : ScenarioForm
exerciseAge2 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "4" }
        ]
        (Exercise
            { title = "The Age header (2)"
            , answers =
                [ { answer = "Varnish returns a response with the same body as before, with an age of 1", selected = False, correct = False }
                , { answer = "Varnish returns a response with the same body as before, with an age of 6", selected = False, correct = True }
                , { answer = "Varnish returns a different response body", selected = False, correct = False }
                ]
                    |> Array.fromList
            }
        )


exerciseAge3 : ScenarioForm
exerciseAge3 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "5" }
        ]
        (Exercise
            { title = "The Age header (3)"
            , answers =
                [ { answer = "Varnish returns a response with the same body as before", selected = False, correct = False }
                , { answer = "Varnish returns a different response body", selected = False, correct = True }
                ]
                    |> Array.fromList
            }
        )


exerciseConditionalRequestsWorkflow1 : ScenarioForm
exerciseConditionalRequestsWorkflow1 =
    create
        { originWait2SecondsBeforeResponding_ = True
        , originReturn304ForConditionalRequests_ = True
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 2)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Exercise
            { title = "Workflow for conditional requests (1)"
            , answers =
                [ { answer = "Varnish returns a 200 with an age of 0 and the same body as before", selected = False, correct = False }
                , { answer = "Varnish returns a 200 with an age of 3 and the same body as before", selected = False, correct = True }
                , { answer = "Varnish returns a 304 with an empty body", selected = False, correct = False }
                ]
                    |> Array.fromList
            }
        )


exerciseConditionalRequestsWorkflow2 : ScenarioForm
exerciseConditionalRequestsWorkflow2 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = True
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 2)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Exercise
            { title = "Workflow for conditional requests (2)"
            , answers =
                [ { answer = "Varnish returns a 200 and also makes a revalidation request", selected = False, correct = False }
                , { answer = "Varnish returns a 200 and does not make a revalidation request", selected = False, correct = True }
                ]
                    |> Array.fromList
            }
        )


exerciseConditionalRequestsWorkflow3 : ScenarioForm
exerciseConditionalRequestsWorkflow3 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([ { key = "If-None-Match", value = "\"some-etag\"" } ] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Exercise
            { title = "Workflow for conditional requests (3)"
            , answers =
                [ { answer = "Varnish returns a 200 and also makes a revalidation request", selected = False, correct = False }
                , { answer = "Varnish returns a 200 and does not make a revalidation request", selected = False, correct = False }
                , { answer = "Varnish returns a 304 and also makes a revalidation request", selected = False, correct = False }
                , { answer = "Varnish returns a 304 and does not make a revalidation request", selected = False, correct = True }
                ]
                    |> Array.fromList
            }
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
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest Array.empty
        , SleepForOneSecond
        , MakeGetRequest Array.empty
        ]
        []
        (Example
            { title = "200 OK responses are cacheable by default"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/8b72bbbc-eb7a-4804-8d72-39bf2519f992",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/8b72bbbc-eb7a-4804-8d72-39bf2519f992",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "65542"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [],
                "body": "1730194626"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "0"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 29 Oct 2024 09:37:06 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65541, 32788"
                    ]
                ],
                "body": "1730194626"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            1
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/8b72bbbc-eb7a-4804-8d72-39bf2519f992",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "1"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 29 Oct 2024 09:37:06 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65541, 33 32789"
                    ]
                ],
                "body": "1730194626"
            }
        ]
    }
]"""
            }
        )


exampleStaleResponsesAreRevalidatedByDefault : ScenarioForm
exampleStaleResponsesAreRevalidatedByDefault =
    create
        { originWait2SecondsBeforeResponding_ = True
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateMaxAge (Just 1)
            |> CacheControl
        ]
        (Example
            { title = "Stale responses are revalidated by default"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/7be02afd-d560-4e3d-b63f-8542b269772c",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/7be02afd-d560-4e3d-b63f-8542b269772c",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "65545"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginSleepingForSeconds",
        "args": [
            2
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "max-age=1"
                    ]
                ],
                "body": "1730194767"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "0"
                    ],
                    [
                        "cache-control",
                        "max-age=1"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 29 Oct 2024 09:39:29 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65544, 32803"
                    ]
                ],
                "body": "1730194767"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            1
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/7be02afd-d560-4e3d-b63f-8542b269772c",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/7be02afd-d560-4e3d-b63f-8542b269772c",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "46"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginSleepingForSeconds",
        "args": [
            2
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "1"
                    ],
                    [
                        "cache-control",
                        "max-age=1"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 29 Oct 2024 09:39:29 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65544, 98309 32804"
                    ]
                ],
                "body": "1730194767"
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "max-age=1"
                    ]
                ],
                "body": "1730194770"
            }
        ]
    }
]"""
            }
        )


exampleCacheVariations : ScenarioForm
exampleCacheVariations =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([ { key = "Accept-Language", value = "en-US" } ] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([ { key = "Accept-Language", value = "zh-CN" } ] |> Array.fromList)
        , MakeGetRequest ([ { key = "Accept-Language", value = "en-US" } ] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "Vary", value = "Accept-Encoding, Accept-Language" }
        ]
        (Example
            { title = "Cache variations"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/141bbc07-be84-4432-9492-0d1c39f20276",
                "headers": [
                    [
                        "Accept-Language",
                        "en-US"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/141bbc07-be84-4432-9492-0d1c39f20276",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "accept-language",
                        "en-US"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "98329"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "vary",
                        "Accept-Encoding, Accept-Language"
                    ]
                ],
                "body": "1730753836"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ]
                ],
                "body": "1730753836"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            1
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/141bbc07-be84-4432-9492-0d1c39f20276",
                "headers": [
                    [
                        "Accept-Language",
                        "zh-CN"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/141bbc07-be84-4432-9492-0d1c39f20276",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "accept-language",
                        "zh-CN"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "163852"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "vary",
                        "Accept-Encoding, Accept-Language"
                    ]
                ],
                "body": "1730753837"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ]
                ],
                "body": "1730753837"
            }
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            3,
            {
                "method": "GET",
                "path": "/ids/141bbc07-be84-4432-9492-0d1c39f20276",
                "headers": [
                    [
                        "Accept-Language",
                        "en-US"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ]
                ],
                "body": "1730753836"
            }
        ]
    }
]"""
            }
        )


exampleAge : ScenarioForm
exampleAge =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "Age", value = "4" }
        ]
        (Example
            { title = "Age sent by origin"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/bd1b056c-3e01-49b2-82b9-15393e12bc5d",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/bd1b056c-3e01-49b2-82b9-15393e12bc5d",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "65539"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5,stale-while-revalidate=5"
                    ],
                    [
                        "age",
                        "4"
                    ]
                ],
                "body": "1730798307"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "4"
                    ],
                    [
                        "cache-control",
                        "s-maxage=5,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 09:18:27 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65538, 32771"
                    ]
                ],
                "body": "1730798307"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            2
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/bd1b056c-3e01-49b2-82b9-15393e12bc5d",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/bd1b056c-3e01-49b2-82b9-15393e12bc5d",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "98306"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5,stale-while-revalidate=5"
                    ],
                    [
                        "age",
                        "4"
                    ]
                ],
                "body": "1730798309"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "6"
                    ],
                    [
                        "cache-control",
                        "s-maxage=5,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 09:18:27 GMT"
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "65538, 17 32772"
                    ]
                ],
                "body": "1730798307"
            }
        ]
    }
]"""
            }
        )


exampleConditionalRequestsWorkflow1 : ScenarioForm
exampleConditionalRequestsWorkflow1 =
    create
        { originWait2SecondsBeforeResponding_ = True
        , originReturn304ForConditionalRequests_ = True
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForTwoSeconds
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 1)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Example
            { title = "Conditional requests (1)"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/910c7755-58b1-443c-b618-98bc91ec516a",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/910c7755-58b1-443c-b618-98bc91ec516a",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "131080"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginSleepingForSeconds",
        "args": [
            2
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "access-control-allow-origin",
                        "*"
                    ],
                    [
                        "cache-control",
                        "s-maxage=1,stale-while-revalidate=5"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ]
                ],
                "body": "1730802663"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "access-control-allow-origin",
                        "*"
                    ],
                    [
                        "age",
                        "0"
                    ],
                    [
                        "cache-control",
                        "s-maxage=1,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 10:31:05 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "131079, 65548"
                    ]
                ],
                "body": "1730802663"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            2
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/910c7755-58b1-443c-b618-98bc91ec516a",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/910c7755-58b1-443c-b618-98bc91ec516a",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "if-none-match",
                        "\\"some-etag\\""
                    ],
                    [
                        "x-varnish",
                        "32832"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginSleepingForSeconds",
        "args": [
            2
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "access-control-allow-origin",
                        "*"
                    ],
                    [
                        "age",
                        "2"
                    ],
                    [
                        "cache-control",
                        "s-maxage=1,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 10:31:05 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "131079, 196613 65549"
                    ]
                ],
                "body": "1730802663"
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 304,
                "headers": [
                    [
                        "access-control-allow-origin",
                        "*"
                    ],
                    [
                        "cache-control",
                        "s-maxage=1,stale-while-revalidate=5"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ]
                ],
                "body": ""
            }
        ]
    }
]"""
            }
        )


exampleConditionalRequestsWorkflow2 : ScenarioForm
exampleConditionalRequestsWorkflow2 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = True
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForThreeSeconds
        , MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 2)
            |> CacheControlResponseDirectives.updateStaleWhileRevalidate (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Example
            { title = "Conditional requests (2)"
            , interactionsJson = """
            [
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/4b864a00-002b-444c-bd66-ae170e7471f3",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/4b864a00-002b-444c-bd66-ae170e7471f3",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "163853"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=2,stale-while-revalidate=5"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ]
                ],
                "body": "1730835691"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "0"
                    ],
                    [
                        "cache-control",
                        "s-maxage=2,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 19:41:31 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "163852, 229381"
                    ]
                ],
                "body": "1730835691"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            3
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/4b864a00-002b-444c-bd66-ae170e7471f3",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/4b864a00-002b-444c-bd66-ae170e7471f3",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "if-none-match",
                        "\\"some-etag\\""
                    ],
                    [
                        "x-varnish",
                        "53"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 304,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=2,stale-while-revalidate=5"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ]
                ],
                "body": ""
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "3"
                    ],
                    [
                        "cache-control",
                        "s-maxage=2,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 19:41:31 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "163852, 65574 229382"
                    ]
                ],
                "body": "1730835691"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            3,
            1
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            4,
            {
                "method": "GET",
                "path": "/ids/4b864a00-002b-444c-bd66-ae170e7471f3",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "1"
                    ],
                    [
                        "cache-control",
                        "s-maxage=2,stale-while-revalidate=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 19:41:34 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "98310 53, 65580"
                    ]
                ],
                "body": "1730835691"
            }
        ]
    }
]"""
            }
        )


exampleConditionalRequestsWorkflow3 : ScenarioForm
exampleConditionalRequestsWorkflow3 =
    create
        { originWait2SecondsBeforeResponding_ = False
        , originReturn304ForConditionalRequests_ = False
        }
        [ MakeGetRequest ([] |> Array.fromList)
        , SleepForOneSecond
        , MakeGetRequest ([ { key = "If-None-Match", value = "\"some-etag\"" } ] |> Array.fromList)
        ]
        [ CacheControlResponseDirectives.empty
            |> CacheControlResponseDirectives.updateSMaxAge (Just 5)
            |> CacheControl
        , Custom { key = "ETag", value = "\"some-etag\"" }
        ]
        (Example
            { title = "Conditional requests (3)"
            , interactionsJson = """
[
    {
        "tag": "ClientToVarnish",
        "args": [
            0,
            {
                "method": "GET",
                "path": "/ids/b0153735-fe5f-4ee6-8e65-bf993689e9aa",
                "headers": []
            }
        ]
    },
    {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": "/ids/b0153735-fe5f-4ee6-8e65-bf993689e9aa",
                "headers": [
                    [
                        "accept",
                        "application/json, text/plain, */*"
                    ],
                    [
                        "user-agent",
                        "axios/1.7.7"
                    ],
                    [
                        "host",
                        "varnish"
                    ],
                    [
                        "accept-encoding",
                        "gzip"
                    ],
                    [
                        "x-varnish",
                        "98313"
                    ]
                ]
            }
        ]
    },
    {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ]
                ],
                "body": "1730837726"
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 200,
                "headers": [
                    [
                        "accept-ranges",
                        "bytes"
                    ],
                    [
                        "age",
                        "0"
                    ],
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-length",
                        "10"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 20:15:26 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "98312, 229413"
                    ]
                ],
                "body": "1730837726"
            }
        ]
    },
    {
        "tag": "ClientSleepingForSeconds",
        "args": [
            1,
            1
        ]
    },
    {
        "tag": "ClientToVarnish",
        "args": [
            2,
            {
                "method": "GET",
                "path": "/ids/b0153735-fe5f-4ee6-8e65-bf993689e9aa",
                "headers": [
                    [
                        "If-None-Match",
                        "\\"some-etag\\""
                    ]
                ]
            }
        ]
    },
    {
        "tag": "VarnishToClient",
        "args": [
            {
                "statusCode": 304,
                "headers": [
                    [
                        "age",
                        "1"
                    ],
                    [
                        "cache-control",
                        "s-maxage=5"
                    ],
                    [
                        "connection",
                        "keep-alive"
                    ],
                    [
                        "content-type",
                        "text/html; charset=utf-8"
                    ],
                    [
                        "date",
                        "Tue, 05 Nov 2024 20:15:26 GMT"
                    ],
                    [
                        "etag",
                        "\\"some-etag\\""
                    ],
                    [
                        "via",
                        "1.1 varnish (Varnish/6.0), 1.1 varnish (Varnish/6.0)"
                    ],
                    [
                        "x-varnish",
                        "54 98313, 229423"
                    ]
                ],
                "body": ""
            }
        ]
    }
]"""
            }
        )


examples : List ScenarioForm
examples =
    [ example200CacheableByDefault
    , exampleStaleResponsesAreRevalidatedByDefault
    , exampleConditionalRequestsWorkflow1
    , exampleConditionalRequestsWorkflow2
    , exampleConditionalRequestsWorkflow3
    , exampleAge
    , exampleCacheVariations
    ]


exampleLinksByTitle : List ( Html msg, String )
exampleLinksByTitle =
    examples
        |> List.filterMap
            (\example ->
                case mode example of
                    Example { title } ->
                        Just ( text title, toRelativeUrl example )

                    _ ->
                        Nothing
            )


exampleInteractionsByRelativeUrl : Dict String Interactions
exampleInteractionsByRelativeUrl =
    examples
        |> List.filterMap
            (\example ->
                case mode example of
                    Example { interactionsJson } ->
                        interactionsJson
                            |> Codec.decodeString Interactions.codec
                            |> Result.toMaybe
                            |> Maybe.map
                                (\interactions1 ->
                                    ( toRelativeUrl example, interactions1 )
                                )

                    _ ->
                        Nothing
            )
        |> Dict.fromList
