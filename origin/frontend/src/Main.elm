port module Main exposing (main)

import Accessibility.Aria
import Accessibility.Key
import Array
import Base64
import Browser exposing (Document, UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Codec
import Components.Button
import Components.RangeSlider
import Data.CacheControlResponseDirectives as CacheControlResponseDirectives exposing (CacheControlResponseDirectives)
import Data.Config as Config
import Data.Interaction as Interaction
import Data.Scenario as Scenario exposing (Scenario, allRequestHeaderKeys, allResponseHeaderKeys)
import Data.SequenceDiagramVisibility exposing (SequenceDiagramVisibility(..))
import Dict
import ElementIds
import Extras.BrowserDom
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html exposing (..)
import Html.Attributes exposing (checked, class, disabled, href, id, name, type_)
import Html.Events
import Http
import Icons
import Interactions exposing (Interactions)
import Language
import Process
import Random
import Regex
import ScenarioForm exposing (ScenarioForm)
import Svg.Attributes
import Task
import Time
import Url exposing (Url)
import Url.Builder
import Uuid



-- MAIN


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Key
    , id : Maybe String
    , scenarioForm : ScenarioForm
    , scenarioIsRunning : Bool
    , interactions : Result Http.Error Interactions
    , formWasModifiedSinceScenarioRun : Bool
    , showAllHeaders : Bool
    , sequenceDiagramVisibility : SequenceDiagramVisibility
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    defaultModel key
        |> update (UrlChanged url)


defaultModel : Key -> Model
defaultModel key =
    { key = key
    , id = Nothing
    , scenarioForm = ScenarioForm.empty
    , scenarioIsRunning = False
    , interactions = Ok Interactions.empty
    , formWasModifiedSinceScenarioRun = False
    , showAllHeaders = False
    , sequenceDiagramVisibility = CompletelyRevealed
    }



-- PORTS


port scrollToBottomOfSequenceDiagram : () -> Cmd msg



-- UPDATE


type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | MakeUrlReflectScenarioForm ScenarioForm
    | AddMakeGetRequest
    | AddMakePurgeRequest
    | AddGetRequestHeader Int
    | AddGetRequestHeaderWithKey Int String
    | AddGetRequestHeaderWithKeyAndValue Int String String
    | UpdateGetRequestHeaderKey Int Int String
    | UpdateGetRequestHeaderValue Int Int String
    | DeleteGetRequestHeader Int Int
    | AddSleepForTwoSeconds
    | ChangeSleepDuration Int Int
    | DeleteClientAction Int
    | ToggleOriginWait2SecondsBeforeResponding
    | ToggleOriginReturn304ForConditionalRequests
    | AddCustomOriginResponseHeader
    | AddOriginResponseHeaderWithKey String
    | AddOriginResponseHeaderWithKeyAndValue String String
    | AddOriginCacheControlHeader
    | ChangeMaxAge Int (Maybe Int)
    | ChangeSMaxAge Int (Maybe Int)
    | ToggleNoStore Int
    | TogglePrivate Int
    | ChangeStaleWhileRevalidate Int (Maybe Int)
    | ChangeCustomCacheControlDirectives Int String
    | DeleteCustomOriginResponseHeader Int
    | UpdateCustomOriginResponseHeaderKey Int String
    | UpdateCustomOriginResponseHeaderValue Int String
    | RunScenarioFromForm
    | RunScenarioAsExerciseFromForm SequenceDiagramVisibility
    | ResetScenarioForm
    | NewUuid String
    | GetInteractions Time.Posix
    | GetInteractionsAfterScenarioHasFinished
    | GotInteractions (Result Http.Error Interactions)
    | ScrollToBottomOfSequenceDiagram
    | RunScenario Scenario
    | RecordedSleepForSeconds Scenario Int (Result Http.Error ())
    | RecordedMakeGetRequest
        Scenario
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
        (Result Http.Error ())
    | GotResponseToGetRequest Scenario (Result Http.Error ( Http.Metadata, String ))
    | RecordedResponseToGetRequest Scenario (Result Http.Error ())
    | RecordedMakePurgeRequest
        Scenario
        { path : String
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
        (Result Http.Error ())
    | GotResponseToPurgeRequest
        Scenario
        { path : String
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
        (Result Http.Error ( Http.Metadata, String ))
    | RecordedResponseToPurgeRequest
        Scenario
        { path : String
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
        (Result Http.Error ())
    | GotResponseToSanitisedPurgeRequest Scenario (Result Http.Error ( Http.Metadata, String ))
    | ToggleShowAllHeaders
    | SelectExerciseAnswer Int
    | SubmitExerciseForm
    | LeaveExercise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- This typically means a link has been clicked
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , url
                        |> Url.toString
                        |> Browser.Navigation.pushUrl model.key
                    )

                Browser.External string ->
                    ( model
                    , Browser.Navigation.load string
                    )

        UrlChanged url ->
            let
                scenarioForm =
                    ScenarioForm.fromUrl url

                exercise =
                    ScenarioForm.exerciseAnswers scenarioForm /= Nothing

                sequenceDiagramVisibility =
                    if exercise then
                        FinalInteractionsConcealedForExercise

                    else
                        CompletelyRevealed

                model0 =
                    if exercise then
                        defaultModel model.key

                    else
                        model
            in
            ( { model0
                | scenarioForm = scenarioForm
                , sequenceDiagramVisibility = sequenceDiagramVisibility
              }
            , if ScenarioForm.autoRun scenarioForm then
                Process.sleep 500
                    |> Task.perform (always <| RunScenarioAsExerciseFromForm sequenceDiagramVisibility)

              else
                Cmd.none
            )

        MakeUrlReflectScenarioForm scenarioForm ->
            if model.scenarioForm == scenarioForm then
                ( model
                , scenarioForm
                    |> ScenarioForm.toRelativeUrl
                    |> Browser.Navigation.pushUrl model.key
                )

            else
                ( model, Cmd.none )

        AddMakeGetRequest ->
            updateScenarioForm
                ScenarioForm.addMakeGetRequest
                []
                model

        AddMakePurgeRequest ->
            updateScenarioForm
                ScenarioForm.addMakePurgeRequest
                []
                model

        AddGetRequestHeader stepIndex ->
            let
                scenarioForm_ =
                    ScenarioForm.addGetRequestHeader stepIndex "" "" model.scenarioForm
            in
            updateScenarioForm
                (always scenarioForm_)
                [ scenarioForm_
                    |> ScenarioForm.indexOfLastCustomHeaderInGetRequest
                    |> Maybe.map
                        (\index_ ->
                            ElementIds.getRequestHeaderKey stepIndex index_
                                |> Browser.Dom.focus
                                |> Task.attempt (always NoOp)
                        )
                    |> Maybe.withDefault Cmd.none
                ]
                model

        AddGetRequestHeaderWithKey stepIndex key ->
            let
                scenarioForm_ =
                    ScenarioForm.addGetRequestHeader stepIndex key "" model.scenarioForm
            in
            updateScenarioForm
                (always scenarioForm_)
                [ scenarioForm_
                    |> ScenarioForm.indexOfLastCustomHeaderInGetRequest
                    |> Maybe.map
                        (\index_ ->
                            ElementIds.getRequestHeaderValue stepIndex index_
                                |> Browser.Dom.focus
                                |> Task.attempt (always NoOp)
                        )
                    |> Maybe.withDefault Cmd.none
                ]
                model

        AddGetRequestHeaderWithKeyAndValue stepIndex key value ->
            updateScenarioForm
                (ScenarioForm.addGetRequestHeader stepIndex key value)
                []
                model

        UpdateGetRequestHeaderKey stepIndex headerIndex newKey ->
            updateScenarioForm
                (ScenarioForm.updateGetRequestHeaderKey stepIndex headerIndex newKey)
                []
                model

        UpdateGetRequestHeaderValue stepIndex headerIndex newValue ->
            updateScenarioForm
                (ScenarioForm.updateGetRequestHeaderValue stepIndex headerIndex newValue)
                []
                model

        DeleteGetRequestHeader stepIndex headerIndex ->
            updateScenarioForm
                (ScenarioForm.deleteGetRequestHeader stepIndex headerIndex)
                []
                model

        AddSleepForTwoSeconds ->
            updateScenarioForm
                ScenarioForm.addSleepForTwoSeconds
                []
                model

        ChangeSleepDuration stepIndex value ->
            let
                newAction =
                    case value of
                        1 ->
                            ScenarioForm.SleepForOneSecond

                        2 ->
                            ScenarioForm.SleepForTwoSeconds

                        3 ->
                            ScenarioForm.SleepForThreeSeconds

                        5 ->
                            ScenarioForm.SleepForFiveSeconds

                        _ ->
                            ScenarioForm.SleepForEightSeconds
            in
            updateScenarioForm
                (ScenarioForm.changeClientAction stepIndex newAction)
                []
                model

        DeleteClientAction stepIndex ->
            updateScenarioForm
                (ScenarioForm.deleteClientAction stepIndex)
                []
                model

        ToggleOriginWait2SecondsBeforeResponding ->
            updateScenarioForm
                ScenarioForm.toggleOriginWait2SecondsBeforeResponding
                []
                model

        ToggleOriginReturn304ForConditionalRequests ->
            updateScenarioForm
                ScenarioForm.toggleOriginReturn304ForConditionalRequests
                []
                model

        AddCustomOriginResponseHeader ->
            let
                scenarioForm_ =
                    ScenarioForm.addOriginCustomHeader "" "" model.scenarioForm
            in
            updateScenarioForm
                (always scenarioForm_)
                [ scenarioForm_
                    |> ScenarioForm.originHeaders
                    |> List.length
                    |> (+) -1
                    |> ElementIds.originCustomHeaderKey
                    |> Browser.Dom.focus
                    |> Task.attempt (always NoOp)
                ]
                model

        AddOriginResponseHeaderWithKey key ->
            let
                scenarioForm_ =
                    ScenarioForm.addOriginCustomHeader key "" model.scenarioForm
            in
            updateScenarioForm
                (always scenarioForm_)
                [ scenarioForm_
                    |> ScenarioForm.originHeaders
                    |> List.length
                    |> (+) -1
                    |> ElementIds.originCustomHeaderValue
                    |> Browser.Dom.focus
                    |> Task.attempt (always NoOp)
                ]
                model

        AddOriginResponseHeaderWithKeyAndValue key value ->
            updateScenarioForm
                (ScenarioForm.addOriginCustomHeader key value)
                []
                model

        AddOriginCacheControlHeader ->
            updateScenarioForm
                ScenarioForm.addOriginCacheControlHeader
                []
                model

        ChangeMaxAge index maybeValue ->
            updateScenarioForm
                (ScenarioForm.changeMaxAge index maybeValue)
                []
                model

        ChangeSMaxAge index maybeValue ->
            updateScenarioForm
                (ScenarioForm.changeSMaxAge index maybeValue)
                []
                model

        ToggleNoStore index ->
            updateScenarioForm
                (ScenarioForm.toggleNoStore index)
                []
                model

        TogglePrivate index ->
            updateScenarioForm
                (ScenarioForm.togglePrivate index)
                []
                model

        ChangeStaleWhileRevalidate index maybeValue ->
            updateScenarioForm
                (ScenarioForm.changeStaleWhileRevalidate index maybeValue)
                []
                model

        ChangeCustomCacheControlDirectives index value ->
            updateScenarioForm
                (ScenarioForm.changeCustomCacheControlDirectives index value)
                []
                model

        DeleteCustomOriginResponseHeader index ->
            updateScenarioForm
                (ScenarioForm.deleteOriginHeader index)
                []
                model

        UpdateCustomOriginResponseHeaderKey index newKey ->
            updateScenarioForm
                (ScenarioForm.updateOriginCustomHeaderKey index newKey)
                []
                model

        UpdateCustomOriginResponseHeaderValue index newValue ->
            updateScenarioForm
                (ScenarioForm.updateOriginCustomHeaderValue index newValue)
                []
                model

        RunScenarioFromForm ->
            ( { model
                | scenarioIsRunning = True
                , interactions = Ok Interactions.empty
                , formWasModifiedSinceScenarioRun = False
              }
            , Random.generate NewUuid Uuid.uuidStringGenerator
            )

        RunScenarioAsExerciseFromForm sequenceDiagramVisibility ->
            ( { model
                | scenarioIsRunning = True
                , interactions = Ok Interactions.empty
                , formWasModifiedSinceScenarioRun = False
                , sequenceDiagramVisibility = sequenceDiagramVisibility
              }
            , Random.generate NewUuid Uuid.uuidStringGenerator
            )

        NewUuid uuid ->
            let
                scenario : Scenario
                scenario =
                    ScenarioForm.toScenario model.scenarioForm uuid
            in
            ( { model | id = Just uuid }
            , runScenario scenario
            )

        GetInteractions _ ->
            ( model
            , model.id
                |> Maybe.map getInteractions
                |> Maybe.withDefault Cmd.none
            )

        GetInteractionsAfterScenarioHasFinished ->
            ( { model | scenarioIsRunning = False }
            , model.id
                |> Maybe.map getInteractions
                |> Maybe.withDefault Cmd.none
            )

        GotInteractions result ->
            ( { model | interactions = result }
            , Process.sleep 100 |> Task.perform (always ScrollToBottomOfSequenceDiagram)
            )

        ScrollToBottomOfSequenceDiagram ->
            ( model, scrollToBottomOfSequenceDiagram () )

        RunScenario scenario ->
            ( model, runScenario scenario )

        RecordedSleepForSeconds restOfScenario seconds _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , seconds
                    |> toFloat
                    |> (*) 1000
                    |> Process.sleep
                    |> Task.perform (always <| RunScenario restOfScenario)
                ]
            )

        RecordedMakeGetRequest restOfScenario { path, headers, desiredResponseHeaders, respondSlowly, auto304 } _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , makeRequest
                    (GotResponseToGetRequest restOfScenario)
                    { method = "GET"
                    , path = path
                    , headers = headers
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
                ]
            )

        GotResponseToGetRequest restOfScenario result ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , result
                    |> Result.map
                        (\( metadata, responseBody ) ->
                            recordResponseToGetRequest restOfScenario metadata responseBody
                        )
                    |> Result.withDefault Cmd.none
                ]
            )

        RecordedResponseToGetRequest restOfScenario _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , runScenario restOfScenario
                ]
            )

        RecordedMakePurgeRequest restOfScenario { path, desiredResponseHeaders, respondSlowly, auto304 } _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , makeRequest
                    (GotResponseToPurgeRequest
                        restOfScenario
                        { path = path
                        , desiredResponseHeaders = desiredResponseHeaders
                        , respondSlowly = respondSlowly
                        , auto304 = auto304
                        }
                    )
                    { method = "PURGE"
                    , path = path
                    , headers = []
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
                ]
            )

        GotResponseToPurgeRequest restOfScenario { path, desiredResponseHeaders, respondSlowly, auto304 } result ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , result
                    |> Result.map
                        (\( metadata, responseBody ) ->
                            recordResponseToPurgeRequest
                                restOfScenario
                                { path = path
                                , desiredResponseHeaders = desiredResponseHeaders
                                , respondSlowly = respondSlowly
                                , auto304 = auto304
                                }
                                metadata
                                responseBody
                        )
                    |> Result.withDefault Cmd.none
                ]
            )

        RecordedResponseToPurgeRequest restOfScenario { path, desiredResponseHeaders, respondSlowly, auto304 } _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , makeRequest
                    (GotResponseToSanitisedPurgeRequest restOfScenario)
                    { method = "POST"
                    , path = "/purge" ++ path
                    , headers = []
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
                ]
            )

        GotResponseToSanitisedPurgeRequest restOfScenario _ ->
            ( model
            , runScenario restOfScenario
            )

        ResetScenarioForm ->
            updateScenarioForm
                (always ScenarioForm.empty)
                [ Extras.BrowserDom.scrollToTop NoOp ]
                model

        ToggleShowAllHeaders ->
            ( { model | showAllHeaders = not model.showAllHeaders }
            , Cmd.none
            )

        SelectExerciseAnswer id ->
            ( { model
                | scenarioForm = model.scenarioForm |> ScenarioForm.selectExerciseAnswer id
              }
            , Cmd.none
            )

        SubmitExerciseForm ->
            ( { model
                | sequenceDiagramVisibility = FinalInteractionsRevealedForExercise
              }
            , Process.sleep 200 |> Task.perform (always ScrollToBottomOfSequenceDiagram)
            )

        LeaveExercise ->
            model.key
                |> defaultModel
                |> update (MakeUrlReflectScenarioForm ScenarioForm.empty)
                |> (\( model_, cmd_ ) ->
                        ( model_
                        , Cmd.batch [ cmd_, Extras.BrowserDom.scrollToTop NoOp ]
                        )
                   )


updateScenarioForm : (ScenarioForm -> ScenarioForm) -> List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
updateScenarioForm f commands model =
    let
        doNotAllowMakingChanges =
            ScenarioForm.exerciseAnswers model.scenarioForm /= Nothing
    in
    if doNotAllowMakingChanges then
        ( model, Cmd.none )

    else
        let
            updatedScenarioForm =
                model.scenarioForm
                    |> f
                    |> ScenarioForm.updateAutoRun False
        in
        ( { model
            | scenarioForm = updatedScenarioForm
            , formWasModifiedSinceScenarioRun = True
          }
        , Cmd.batch
            ((Process.sleep 1000
                |> Task.perform
                    (always <|
                        MakeUrlReflectScenarioForm updatedScenarioForm
                    )
             )
                :: commands
            )
        )


originUrl : List String -> List Url.Builder.QueryParameter -> String
originUrl =
    Url.Builder.crossOrigin Config.originBaseUrl


getInteractions : String -> Cmd Msg
getInteractions id =
    let
        url =
            originUrl
                [ "interactions", id ]
                []
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotInteractions (Codec.decoder Interactions.codec)
        }


expectWhateverResponse : (Result Http.Error ( Http.Metadata, String ) -> msg) -> Http.Expect msg
expectWhateverResponse toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    if metadata.statusCode == 304 then
                        Ok ( metadata, "" )

                    else
                        Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok ( metadata, body )


runScenario : Scenario -> Cmd Msg
runScenario scenario =
    scenario
        |> Scenario.nextAction
        |> Maybe.map
            (\( ( stepIndex, action ), restOfScenario ) ->
                case action of
                    Scenario.SleepForSeconds seconds ->
                        recordSleepForSeconds restOfScenario stepIndex seconds

                    Scenario.MakeGetRequest { path, headers, desiredResponseHeaders, respondSlowly, auto304 } ->
                        recordMakeGetRequest
                            restOfScenario
                            stepIndex
                            { path = path
                            , headers = headers
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = respondSlowly
                            , auto304 = auto304
                            }

                    Scenario.MakePurgeRequest { path, desiredResponseHeaders, respondSlowly, auto304 } ->
                        recordMakePurgeRequest restOfScenario
                            stepIndex
                            { path = path
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = respondSlowly
                            , auto304 = auto304
                            }
            )
        |> Maybe.withDefault
            (Process.sleep 2500
                |> Task.perform (always GetInteractionsAfterScenarioHasFinished)
            )


recordSleepForSeconds : Scenario -> Int -> Int -> Cmd Msg
recordSleepForSeconds restOfScenario stepIndex seconds =
    let
        id =
            Scenario.id restOfScenario

        url =
            originUrl
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.ClientSleepingForSeconds stepIndex seconds
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect = Http.expectWhatever <| RecordedSleepForSeconds restOfScenario seconds
        }


recordMakeGetRequest :
    Scenario
    -> Int
    ->
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
    -> Cmd Msg
recordMakeGetRequest restOfScenario stepIndex { path, headers, desiredResponseHeaders, respondSlowly, auto304 } =
    let
        id =
            Scenario.id restOfScenario

        url =
            originUrl
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.ClientToVarnish stepIndex
                { method = "GET", path = path, headers = headers }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect =
            Http.expectWhatever <|
                RecordedMakeGetRequest restOfScenario
                    { path = path
                    , headers = headers
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
        }


makeRequest :
    (Result Http.Error ( Http.Metadata, String ) -> Msg)
    ->
        { method : String
        , path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
    -> Cmd Msg
makeRequest onResponse { method, path, headers, desiredResponseHeaders, respondSlowly, auto304 } =
    let
        queryParametersForHeadersToSend : List Url.Builder.QueryParameter
        queryParametersForHeadersToSend =
            headers
                |> List.map
                    (\( key, value ) ->
                        (key ++ ":" ++ value)
                            |> Base64.encode
                            |> Url.Builder.string "headers-to-send"
                    )

        queryParametersForHeadersToReturn : List Url.Builder.QueryParameter
        queryParametersForHeadersToReturn =
            desiredResponseHeaders
                |> List.map
                    (\( key, value ) ->
                        (key ++ ":" ++ value)
                            |> Base64.encode
                            |> Url.Builder.string "headers-to-return"
                    )

        url : String
        url =
            let
                leadingSlashRegex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString "^/"

                pathWithoutLeadingSlash =
                    Regex.replace leadingSlashRegex (always "") path
            in
            originUrl
                [ pathWithoutLeadingSlash ]
                (queryParametersForHeadersToSend
                    ++ queryParametersForHeadersToReturn
                    ++ (if respondSlowly then
                            [ Url.Builder.string "respond-slowly" "" ]

                        else
                            []
                       )
                    ++ (if auto304 then
                            [ Url.Builder.string "auto-304" "" ]

                        else
                            []
                       )
                )

        headerToAvoidBrowserCaching =
            Http.header "Cache-Control" "no-cache, no-store"

        httpHeaders =
            headers
                |> List.filter (\( key, _ ) -> String.toLower key /= "cache-control")
                |> List.map (\( key, value ) -> Http.header key value)
    in
    Http.request
        { method = method
        , headers = headerToAvoidBrowserCaching :: httpHeaders
        , url = url
        , body = Http.emptyBody
        , expect = expectWhateverResponse onResponse
        , timeout = Nothing
        , tracker = Nothing
        }


recordResponseToGetRequest : Scenario -> Http.Metadata -> String -> Cmd Msg
recordResponseToGetRequest restOfScenario metadata responseBody =
    let
        id =
            Scenario.id restOfScenario

        url =
            originUrl
                [ "interactions", id, "new" ]
                []

        leadingXVcp =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^x-vcp-"
    in
    Http.post
        { url = url
        , body =
            Interaction.VarnishToClient
                { statusCode = metadata.statusCode
                , headers =
                    metadata.headers
                        |> Dict.toList
                        |> List.map (Tuple.mapFirst (Regex.replace leadingXVcp <| always ""))
                , body = responseBody
                }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect = Http.expectWhatever <| RecordedResponseToGetRequest restOfScenario
        }


recordMakePurgeRequest :
    Scenario
    -> Int
    ->
        { path : String
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
    -> Cmd Msg
recordMakePurgeRequest restOfScenario stepIndex { path, desiredResponseHeaders, respondSlowly, auto304 } =
    let
        id =
            Scenario.id restOfScenario

        url =
            originUrl
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.ClientToVarnish stepIndex
                { method = "PURGE", path = path, headers = [] }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect =
            Http.expectWhatever <|
                RecordedMakePurgeRequest restOfScenario
                    { path = path
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
        }


recordResponseToPurgeRequest :
    Scenario
    ->
        { path : String
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
    -> Http.Metadata
    -> String
    -> Cmd Msg
recordResponseToPurgeRequest restOfScenario { path, desiredResponseHeaders, respondSlowly, auto304 } metadata responseBody =
    let
        id =
            Scenario.id restOfScenario

        url =
            originUrl
                [ "interactions", id, "new" ]
                []

        leadingXVcp =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^x-vcp-"
    in
    Http.post
        { url = url
        , body =
            Interaction.VarnishToClient
                { statusCode = metadata.statusCode
                , headers =
                    metadata.headers
                        |> Dict.toList
                        |> List.map (Tuple.mapFirst (Regex.replace leadingXVcp <| always ""))
                , body = responseBody
                }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect =
            Http.expectWhatever <|
                RecordedResponseToPurgeRequest
                    restOfScenario
                    { path = path
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    , auto304 = auto304
                    }
        }



-- VIEW


viewAddHeaderButton : List (Attribute Msg) -> Bool -> String -> Msg -> Html Msg
viewAddHeaderButton attributes enabled title onClick =
    button
        ([ class "btn"
         , Html.Events.onClick onClick
         , Html.Attributes.disabled <| not enabled
         ]
            ++ attributes
        )
        [ Icons.plus [ Svg.Attributes.class "h-5 w-5" ]
        , text title
        ]


isForbiddenHeaderName : String -> Bool
isForbiddenHeaderName name =
    let
        nameLower =
            String.toLower name
    in
    List.member nameLower
        [ "accept-charset"
        , "accept-encoding"
        , "access-control-request-headers"
        , "access-control-request-method"
        , "connection"
        , "content-length"
        , "date"
        , "dnt"
        , "expect"
        , "host"
        , "keep-alive"
        , "origin"
        , "permissions-policy"
        , "referer"
        , "te"
        , "trailer"
        , "transfer-encoding"
        , "upgrade"
        , "via"
        ]
        || String.startsWith "proxy-" nameLower
        || String.startsWith "sec-" nameLower


viewGetRequestHeader : Bool -> Int -> Int -> ScenarioForm.Header -> Html Msg
viewGetRequestHeader enabled stepIndex index header =
    let
        keyIsForbidden =
            isForbiddenHeaderName header.key
    in
    div
        [ class "flex-auto max-w-2xl flex" ]
        [ button
            [ class "btn btn-circle mr-3"
            , Accessibility.Aria.label "Delete"
            , Html.Attributes.disabled <| not enabled
            , Html.Events.onClick <| DeleteGetRequestHeader stepIndex index
            ]
            [ Icons.trash
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        , div
            [ class "flex-auto" ]
            [ div
                [ class "flex flex-row items-center" ]
                [ input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.placeholder "key"
                    , Html.Attributes.class "input input-bordered w-full"
                    , Html.Attributes.id <| ElementIds.getRequestHeaderKey stepIndex index
                    , Extras.HtmlAttribute.showIf keyIsForbidden <| Html.Attributes.class "input-error"
                    , Html.Attributes.attribute "autocorrect" "off"
                    , Html.Attributes.attribute "autocapitalize" "off"
                    , Html.Attributes.spellcheck False
                    , Html.Attributes.autocomplete False
                    , Html.Attributes.disabled <| not enabled
                    , Accessibility.Aria.required True
                    , Accessibility.Aria.label "Key"
                    , Html.Attributes.value header.key
                    , Html.Events.onInput (UpdateGetRequestHeaderKey stepIndex index)
                    , Extras.HtmlEvents.onEnter NoOp
                    ]
                    []
                , span
                    [ class "ml-1 mr-2" ]
                    [ text " : " ]
                , input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.placeholder "value"
                    , Html.Attributes.class "input input-bordered w-full"
                    , Html.Attributes.id <| ElementIds.getRequestHeaderValue stepIndex index
                    , Html.Attributes.attribute "autocorrect" "off"
                    , Html.Attributes.attribute "autocapitalize" "off"
                    , Html.Attributes.spellcheck False
                    , Html.Attributes.autocomplete False
                    , Html.Attributes.disabled <| not enabled
                    , Accessibility.Aria.required True
                    , Accessibility.Aria.label "Value"
                    , Html.Attributes.value header.value
                    , Html.Events.onInput (UpdateGetRequestHeaderValue stepIndex index)
                    , Extras.HtmlEvents.onEnter NoOp
                    ]
                    []
                ]
            , Extras.Html.showIf keyIsForbidden <|
                div
                    [ class "mt-2 text-red-700" ]
                    [ text "Setting this header is "
                    , Extras.Html.externalLink "https://developer.mozilla.org/en-US/docs/Glossary/Forbidden_header_name" [ text "not supported" ]
                    , text " (although cookies can be)."
                    ]
            , Extras.Html.showIf (String.toLower header.key == "cache-control") <|
                div
                    [ class "mt-2 text-red-700" ]
                    [ text "Setting this header is not supported as it may cause confusion with browser caching." ]
            ]
        ]


viewOriginHeader : Bool -> Int -> ScenarioForm.OriginHeader -> Html Msg
viewOriginHeader enabled index originHeader =
    div
        [ class "flex-auto max-w-2xl flex items-center" ]
        [ button
            [ class "btn btn-circle mr-3"
            , Accessibility.Aria.label "Delete"
            , Html.Attributes.disabled <| not enabled
            , Html.Events.onClick <| DeleteCustomOriginResponseHeader index
            ]
            [ Icons.trash
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        , case originHeader of
            ScenarioForm.CacheControl cacheControlResponseDirectives ->
                viewOriginCacheControlHeader enabled index cacheControlResponseDirectives

            ScenarioForm.Custom header ->
                viewOriginCustomHeader enabled index header
        ]


viewOriginCustomHeader : Bool -> Int -> ScenarioForm.Header -> Html Msg
viewOriginCustomHeader enabled index header =
    div
        [ class "flex-auto" ]
        [ div
            [ class "flex flex-row items-center" ]
            [ input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "key"
                , Html.Attributes.class "input input-bordered w-full"
                , Html.Attributes.attribute "autocorrect" "off"
                , Html.Attributes.attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                , Html.Attributes.autocomplete False
                , Html.Attributes.disabled <| not enabled
                , Html.Attributes.id <| ElementIds.originCustomHeaderKey index
                , Accessibility.Aria.required True
                , Accessibility.Aria.label "Key"
                , Html.Attributes.value header.key
                , Html.Events.onInput (UpdateCustomOriginResponseHeaderKey index)
                , Extras.HtmlEvents.onEnter NoOp
                ]
                []
            , span
                [ class "ml-1 mr-2" ]
                [ text " : " ]
            , input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "value"
                , Html.Attributes.class "input input-bordered w-full"
                , Html.Attributes.attribute "autocorrect" "off"
                , Html.Attributes.attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                , Html.Attributes.autocomplete False
                , Html.Attributes.disabled <| not enabled
                , Html.Attributes.id <| ElementIds.originCustomHeaderValue index
                , Accessibility.Aria.required True
                , Accessibility.Aria.label "Value"
                , Html.Attributes.value header.value
                , Html.Events.onInput (UpdateCustomOriginResponseHeaderValue index)
                , Extras.HtmlEvents.onEnter NoOp
                ]
                []
            ]
        , Extras.Html.showIf (String.toLower header.key == "etag") <|
            div
                [ class "mt-2" ]
                [ button
                    [ class "btn btn-sm"
                    , Html.Events.onClick <| UpdateCustomOriginResponseHeaderValue index "auto"
                    , Html.Attributes.disabled (not enabled || header.value == "auto")
                    ]
                    [ text "Auto-generate ETags" ]
                ]
        ]


viewOriginCacheControlHeader : Bool -> Int -> CacheControlResponseDirectives -> Html Msg
viewOriginCacheControlHeader enabled index directives =
    let
        maxAge =
            CacheControlResponseDirectives.maxAge directives

        sMaxAge =
            CacheControlResponseDirectives.sMaxAge directives

        noStore =
            CacheControlResponseDirectives.noStore directives

        private =
            CacheControlResponseDirectives.private directives

        staleWhileRevalidate =
            CacheControlResponseDirectives.staleWhileRevalidate directives

        customDirectives =
            CacheControlResponseDirectives.custom directives

        headerValue =
            CacheControlResponseDirectives.toString False directives
    in
    div
        [ class "w-full" ]
        [ fieldset
            [ class "border border-solid rounded-md border-gray-300 px-4 py-2" ]
            [ legend
                [ class "px-3 py-0.5 select-none" ]
                [ text "Cache-Control" ]
            , div
                [ class "flex flex-wrap" ]
                [ viewOriginCacheControlMaxAgeDirective enabled index maxAge
                , viewOriginCacheControlSMaxAgeDirective enabled index sMaxAge
                , viewOriginCacheControlNoStoreDirective enabled index noStore
                , viewOriginCacheControlPrivateDirective enabled index private
                , viewOriginCacheControlStaleWhileRevalidateDirective enabled index staleWhileRevalidate
                , viewCustomDirectives enabled index customDirectives
                ]
            , div
                [ class "mt-4 mb-2 text-gray-700 text-ellipsis text-nowrap overflow-hidden max-w-52 sm:max-w-96 md:max-w-96 text-sm" ]
                [ span
                    []
                    [ text "Preview: "
                    , text <|
                        if String.isEmpty headerValue then
                            "[none]"

                        else
                            ""
                    ]
                , span
                    [ class "font-mono" ]
                    [ text headerValue ]
                ]
            ]
        ]


viewCustomDirectives : Bool -> Int -> String -> Html Msg
viewCustomDirectives enabled index value =
    div
        [ class "" ]
        [ div
            [ class "ml-1 mt-4 max-w-full" ]
            [ input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "custom directives"
                , Html.Attributes.class "input input-bordered w-full"
                , Html.Attributes.id <| ElementIds.originCacheControlCustomDirectives index
                , Html.Attributes.attribute "autocorrect" "off"
                , Html.Attributes.attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                , Html.Attributes.autocomplete False
                , Html.Attributes.disabled <| not enabled
                , Html.Attributes.value value
                , Html.Events.onInput (ChangeCustomCacheControlDirectives index)
                , Extras.HtmlEvents.onEnter NoOp
                ]
                []
            ]
        ]


viewOriginCacheControlMaxAgeDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlMaxAgeDirective enabled index maxAgeSeconds =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| maxAgeSeconds /= Nothing
            , class "checkbox"
            , disabled <| not enabled
            , Html.Attributes.id <| ElementIds.originCacheControlMaxAge index
            , Html.Events.onClick
                (if maxAgeSeconds == Nothing then
                    ChangeMaxAge index (Just 0)

                 else
                    ChangeMaxAge index Nothing
                )
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showIf enabled <| class "cursor-pointer"
            , Extras.HtmlAttribute.showIf enabled <|
                Html.Events.onClick
                    (if maxAgeSeconds == Nothing then
                        ChangeMaxAge index (Just 0)

                     else
                        ChangeMaxAge index Nothing
                    )
            ]
            [ span
                [ class "font-mono" ]
                [ text "max-age" ]
            , text "(seconds)"
            ]
        , Components.RangeSlider.viewSeconds
            enabled
            [ 0, 1, 2, 3, 5 ]
            (ChangeMaxAge index << Just)
            (maxAgeSeconds |> Maybe.withDefault 0)
        ]


viewOriginCacheControlSMaxAgeDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlSMaxAgeDirective enabled index sMaxAgeSeconds =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| sMaxAgeSeconds /= Nothing
            , class "checkbox"
            , disabled <| not enabled
            , Html.Attributes.id <| ElementIds.originCacheControlSMaxAge index
            , Html.Events.onClick
                (if sMaxAgeSeconds == Nothing then
                    ChangeSMaxAge index (Just 0)

                 else
                    ChangeSMaxAge index Nothing
                )
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showIf enabled <| class "cursor-pointer"
            , Extras.HtmlAttribute.showIf enabled <|
                Html.Events.onClick
                    (if sMaxAgeSeconds == Nothing then
                        ChangeSMaxAge index (Just 0)

                     else
                        ChangeSMaxAge index Nothing
                    )
            ]
            [ span
                [ class "font-mono" ]
                [ text "s-maxage" ]
            , text "(seconds)"
            ]
        , Components.RangeSlider.viewSeconds
            enabled
            [ 0, 1, 2, 3, 5 ]
            (ChangeSMaxAge index << Just)
            (sMaxAgeSeconds |> Maybe.withDefault 0)
        ]


viewOriginCacheControlNoStoreDirective : Bool -> Int -> Bool -> Html Msg
viewOriginCacheControlNoStoreDirective enabled index noStore =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked noStore
            , class "checkbox"
            , disabled <| not enabled
            , Html.Attributes.id <| ElementIds.originCacheControlNoStore index
            , Html.Events.onClick <| ToggleNoStore index
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showIf enabled <| class "cursor-pointer"
            , Extras.HtmlAttribute.showIf enabled <| Html.Events.onClick <| ToggleNoStore index
            ]
            [ span
                [ class "font-mono" ]
                [ text "no-store" ]
            ]
        ]


viewOriginCacheControlPrivateDirective : Bool -> Int -> Bool -> Html Msg
viewOriginCacheControlPrivateDirective enabled index private =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked private
            , class "checkbox"
            , disabled <| not enabled
            , Html.Attributes.id <| ElementIds.originCacheControlPrivate index
            , Html.Events.onClick <| TogglePrivate index
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showIf enabled <| class "cursor-pointer"
            , Extras.HtmlAttribute.showIf enabled <| Html.Events.onClick <| TogglePrivate index
            ]
            [ span
                [ class "font-mono" ]
                [ text "private" ]
            ]
        ]


viewOriginCacheControlStaleWhileRevalidateDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlStaleWhileRevalidateDirective enabled index staleWhileRevalidate =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| staleWhileRevalidate /= Nothing
            , class "checkbox"
            , disabled <| not enabled
            , Html.Attributes.id <| ElementIds.originCacheControlStaleWhileRevalidate index
            , Html.Events.onClick
                (if staleWhileRevalidate == Nothing then
                    ChangeStaleWhileRevalidate index (Just 0)

                 else
                    ChangeStaleWhileRevalidate index Nothing
                )
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showIf enabled <| class "cursor-pointer"
            , Extras.HtmlAttribute.showIf enabled <|
                Html.Events.onClick
                    (if staleWhileRevalidate == Nothing then
                        ChangeStaleWhileRevalidate index (Just 0)

                     else
                        ChangeStaleWhileRevalidate index Nothing
                    )
            ]
            [ span
                [ class "font-mono" ]
                [ text "stale-while-revalidate" ]
            , text "(seconds)"
            ]
        , Components.RangeSlider.viewSeconds
            enabled
            [ 0, 1, 2, 3, 5 ]
            (ChangeStaleWhileRevalidate index << Just)
            (staleWhileRevalidate |> Maybe.withDefault 0)
        ]


viewClientAction : Bool -> Int -> ScenarioForm.ClientAction -> Html Msg
viewClientAction enabled stepIndex clientAction =
    case clientAction of
        ScenarioForm.MakeGetRequest headers ->
            li
                [ class "rounded-lg bg-white border border-gray-300 shadow" ]
                [ div
                    [ class "px-4 pt-3 text-gray-600" ]
                    [ span
                        [ class "badge badge-ghost text-base" ]
                        [ span
                            [ class "hidden sm:inline sm:mr-1" ]
                            [ text "Step" ]
                        , text <| String.fromInt <| stepIndex + 1
                        ]
                    , span
                        [ class "tooltip tooltip-bottom"
                        , Html.Attributes.attribute "data-tip" "A scenario-wide value for :id is randomly generated when run."
                        ]
                        [ span
                            [ class "ml-2 font-mono" ]
                            [ text "GET /ids/:id" ]
                        ]
                    ]
                , details
                    [ class "px-4 mt-6"
                    , Extras.HtmlAttribute.showIf (not <| Array.isEmpty headers) <| Html.Attributes.attribute "open" "true"
                    ]
                    [ summary
                        [ class "select-none" ]
                        [ span
                            [ class "ml-2 font-medium text-gray-700" ]
                            [ text "Request headers" ]
                        ]
                    , div
                        [ class "space-y-4 px-4 mt-4 max-w-full" ]
                        (headers
                            |> Array.toList
                            |> List.indexedMap (viewGetRequestHeader enabled stepIndex)
                        )
                    , div
                        [ class "ml-4 mt-6" ]
                        [ viewAddHeaderButton
                            [ class "mb-3 mr-3" ]
                            (enabled
                                && not (ScenarioForm.clientActionHasGetRequestHeaderWithKey "If-None-Match" clientAction)
                            )
                            "Add If-None-Match"
                            (AddGetRequestHeaderWithKeyAndValue stepIndex "If-None-Match" "\"some-etag\"")
                        , viewAddHeaderButton
                            [ class "mb-3 mr-3" ]
                            (enabled
                                && not (ScenarioForm.clientActionHasGetRequestHeaderWithKey "If-Modified-Since" clientAction)
                            )
                            "Add If-Modified-Since"
                            (AddGetRequestHeaderWithKeyAndValue stepIndex "If-Modified-Since" "Wed, 21 Oct 2015 07:28:00 GMT")
                        , viewAddHeaderButton
                            []
                            enabled
                            "Add custom"
                            (AddGetRequestHeader stepIndex)
                        ]
                    ]
                , div
                    [ class "pr-3 pb-2" ]
                    [ span
                        [ class "flex justify-end group" ]
                        [ Components.Button.text
                            enabled
                            [ Accessibility.Key.tabbable <| enabled
                            , Html.Events.onClick <| DeleteClientAction stepIndex
                            ]
                            [ Icons.trash
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300"
                                , if enabled then
                                    Svg.Attributes.class "group-hover:text-gray-500 dark:group-hover:text-gray-400"

                                  else
                                    Svg.Attributes.class ""
                                ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300"
                                , if enabled then
                                    class "group-hover:text-gray-700 dark:group-hover:text-gray-400"

                                  else
                                    class ""
                                ]
                                [ text "Delete" ]
                            ]
                        ]
                    ]
                ]

        ScenarioForm.MakePurgeRequest ->
            li
                [ class "rounded-lg bg-white border border-gray-300 shadow" ]
                [ div
                    [ class "px-4 pt-3 text-gray-600" ]
                    [ span
                        [ class "badge badge-ghost text-base" ]
                        [ span
                            [ class "hidden sm:inline sm:mr-1" ]
                            [ text "Step" ]
                        , text <| String.fromInt <| stepIndex + 1
                        ]
                    , span
                        [ class "ml-2 font-mono" ]
                        [ text "PURGE /ids/:id" ]
                    ]
                , div
                    [ class "pr-3 pb-2" ]
                    [ span
                        [ class "flex justify-end group" ]
                        [ Components.Button.text
                            enabled
                            [ Accessibility.Key.tabbable <| enabled
                            , Html.Events.onClick <| DeleteClientAction stepIndex
                            ]
                            [ Icons.trash
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300"
                                , if enabled then
                                    Svg.Attributes.class "group-hover:text-gray-500 dark:group-hover:text-gray-400"

                                  else
                                    Svg.Attributes.class ""
                                ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300"
                                , if enabled then
                                    class "group-hover:text-gray-700 dark:group-hover:text-gray-400"

                                  else
                                    class ""
                                ]
                                [ text "Delete" ]
                            ]
                        ]
                    ]
                ]

        ScenarioForm.SleepForOneSecond ->
            viewSleepForSeconds enabled stepIndex 1

        ScenarioForm.SleepForTwoSeconds ->
            viewSleepForSeconds enabled stepIndex 2

        ScenarioForm.SleepForThreeSeconds ->
            viewSleepForSeconds enabled stepIndex 3

        ScenarioForm.SleepForFiveSeconds ->
            viewSleepForSeconds enabled stepIndex 5

        ScenarioForm.SleepForEightSeconds ->
            viewSleepForSeconds enabled stepIndex 8


viewSleepForSeconds : Bool -> Int -> Int -> Html Msg
viewSleepForSeconds enabled stepIndex seconds =
    li
        [ class "rounded-lg bg-white border border-gray-300 shadow" ]
        [ div
            [ class "px-4 pt-3 text-gray-800 flex" ]
            [ div
                [ class "mt-0.5 badge badge-ghost text-base" ]
                [ span
                    [ class "hidden sm:inline sm:mr-1" ]
                    [ text "Step" ]
                , text <| String.fromInt <| stepIndex + 1
                ]
            , span
                [ class "ml-2"
                , Extras.HtmlAttribute.showIf (seconds == 1) <| class "mr-3"
                ]
                [ text "Sleep "
                , text <| Language.seconds seconds
                ]
            , Components.RangeSlider.viewSeconds
                enabled
                [ 1, 2, 3, 5, 8 ]
                (ChangeSleepDuration stepIndex)
                seconds
            ]
        , div
            [ class "pr-3 pb-2" ]
            [ span
                [ class "flex justify-end group" ]
                [ Components.Button.text
                    enabled
                    [ Accessibility.Key.tabbable <| enabled
                    , Html.Events.onClick <| DeleteClientAction stepIndex
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300"
                        , if enabled then
                            Svg.Attributes.class "group-hover:text-gray-500 dark:group-hover:text-gray-400"

                          else
                            Svg.Attributes.class ""
                        ]
                    , span
                        [ class "font-medium text-gray-600 dark:text-gray-300"
                        , if enabled then
                            class "group-hover:text-gray-700 dark:group-hover:text-gray-400"

                          else
                            class ""
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]


viewScenarioForm : Model -> Html Msg
viewScenarioForm model =
    let
        doingAnExercise =
            ScenarioForm.exerciseTitle model.scenarioForm /= Nothing
    in
    div []
        [ case ScenarioForm.exerciseTitle model.scenarioForm of
            Just exerciseTitle ->
                div
                    [ class "mt-8" ]
                    [ h3
                        [ class "text-lg font-semibold leading-6 text-gray-900" ]
                        [ text <| "Exercise: " ++ exerciseTitle ]
                    , p
                        [ class "mt-2 max-w-4xl text-gray-500" ]
                        [ text "Read through the scenario below, then predict what happens after the last client request." ]
                    ]

            Nothing ->
                p
                    [ class "mt-8 text-gray-600" ]
                    [ text "Explore how "
                    , a
                        [ class "hover:underline"
                        , href "https://varnish-cache.org/"
                        , Html.Attributes.target "_blank"
                        , Html.Attributes.rel "noopener noreferrer"
                        ]
                        [ text "Varnish Cache" ]
                    , text " reacts to "
                    , a
                        [ class "hover:underline"
                        , href "https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching"
                        , Html.Attributes.target "_blank"
                        , Html.Attributes.rel "noopener noreferrer"
                        ]
                        [ text "HTTP headers" ]
                    , text "."
                    , br [] []
                    , text "Prepare a scenario, then run it with a randomly generated "
                    , span
                        [ class "font-mono" ]
                        [ text ":id" ]
                    , text " path parameter."
                    ]
        , Extras.Html.showUnless doingAnExercise <|
            details
                [ class "mt-6" ]
                [ summary
                    [ class "select-none" ]
                    [ span
                        [ class "ml-2 text-lg leading-6 font-medium text-gray-900" ]
                        [ text "Examples" ]
                    ]
                , ul
                    [ class "mt-4 space-y-1.5" ]
                    (ScenarioForm.exampleLinksByTitle
                        |> List.map
                            (\( title, link ) ->
                                a
                                    [ class "text-gray-700 underline"
                                    , href link
                                    ]
                                    [ title ]
                            )
                        |> List.intersperse (span [ class "mx-2.5" ] [ text "·" ])
                    )
                ]
        , Extras.Html.showIf doingAnExercise <| viewOriginSettingsForExercise model
        , Extras.Html.showUnless doingAnExercise <|
            div
                [ class "mt-8 grid grid-cols-1 lg:gap-12 lg:grid-cols-2" ]
                [ div
                    [ class "space-y-8" ]
                    [ h2
                        [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                        [ text "Client" ]
                    , Extras.Html.showIf
                        (model.scenarioForm
                            |> ScenarioForm.clientActions
                            |> Array.isEmpty
                        )
                      <|
                        p
                            [ class "mt-4 text-red-800" ]
                            [ text "[Add one or more steps to create a scenario.]" ]
                    , ul
                        [ Html.Attributes.attribute "role" "list"
                        , class "space-y-4"
                        ]
                        (model.scenarioForm
                            |> ScenarioForm.clientActions
                            |> Array.toList
                            |> List.indexedMap
                                (\index action ->
                                    viewClientAction (not model.scenarioIsRunning) index action
                                )
                        )
                    , div
                        []
                        [ button
                            [ class "btn mr-4 mb-4"
                            , Html.Attributes.disabled <|
                                ((model.scenarioIsRunning
                                    || ScenarioForm.hasTenClientActions model.scenarioForm
                                 )
                                    && model.sequenceDiagramVisibility
                                    /= FinalInteractionsConcealedForExercise
                                )
                            , Html.Events.onClick AddMakeGetRequest
                            ]
                            [ Icons.plus [ Svg.Attributes.class "h-5 w-5" ]
                            , text "Add GET request step"
                            ]
                        , button
                            [ class "btn mr-4 mb-4"
                            , Html.Attributes.disabled <|
                                (model.scenarioIsRunning
                                    || (Array.isEmpty <| ScenarioForm.clientActions model.scenarioForm)
                                    || ScenarioForm.hasTenClientActions model.scenarioForm
                                )
                            , Html.Events.onClick AddSleepForTwoSeconds
                            ]
                            [ Icons.plus [ Svg.Attributes.class "h-5 w-5" ]
                            , text "Add sleep step"
                            ]
                        , button
                            [ class "btn"
                            , Extras.HtmlAttribute.showUnless Config.showButtonForAddingPurgeRequestStep <| class "hidden"
                            , Html.Attributes.disabled <|
                                ((model.scenarioIsRunning
                                    || ScenarioForm.hasTenClientActions model.scenarioForm
                                 )
                                    && model.sequenceDiagramVisibility
                                    /= FinalInteractionsConcealedForExercise
                                )
                            , Html.Events.onClick AddMakePurgeRequest
                            ]
                            [ Icons.plus [ Svg.Attributes.class "h-5 w-5" ]
                            , text "Add PURGE request step"
                            ]
                        ]
                    ]
                , div
                    [ class "space-y-4 mt-8 lg:mt-0" ]
                    [ h2
                        [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100 mb-8" ]
                        [ text "Origin" ]
                    , div
                        [ class "space-y-4" ]
                        [ h2
                            [ class "font-medium text-gray-900 dark:text-gray-100" ]
                            [ text "Response headers"
                            , span
                                [ class "font-normal text-gray-700" ]
                                [ text " for " ]
                            , span
                                [ class "font-mono text-gray-700" ]
                                [ text "/ids/:id" ]
                            ]
                        , div
                            [ class "space-y-4" ]
                            (model.scenarioForm
                                |> ScenarioForm.originHeaders
                                |> List.indexedMap (viewOriginHeader <| not model.scenarioIsRunning)
                            )
                        , div
                            [ class "pt-4" ]
                            [ viewAddHeaderButton
                                [ class "mb-3 mr-3" ]
                                (not model.scenarioIsRunning
                                    && not (ScenarioForm.hasOriginCacheControlHeader model.scenarioForm)
                                )
                                "Add Cache-Control"
                                AddOriginCacheControlHeader
                            , viewAddHeaderButton
                                [ class "mb-3 mr-3" ]
                                (not model.scenarioIsRunning
                                    && not (ScenarioForm.hasCustomOriginHeaderWithKey "ETag" model.scenarioForm)
                                )
                                "Add ETag"
                                (AddOriginResponseHeaderWithKeyAndValue "ETag" "\"some-etag\"")
                            , viewAddHeaderButton
                                [ class "mb-3 mr-3" ]
                                (not model.scenarioIsRunning
                                    && not (ScenarioForm.hasCustomOriginHeaderWithKey "Last-Modified" model.scenarioForm)
                                )
                                "Add Last-Modified"
                                (AddOriginResponseHeaderWithKeyAndValue "Last-Modified" "Wed, 21 Oct 2015 07:28:00 GMT")
                            , viewAddHeaderButton
                                [ class "mb-3 mr-3" ]
                                (not model.scenarioIsRunning
                                    && not (ScenarioForm.hasCustomOriginHeaderWithKey "Vary" model.scenarioForm)
                                )
                                "Add Vary"
                                (AddOriginResponseHeaderWithKeyAndValue "Vary" "Accept-Encoding")
                            , viewAddHeaderButton
                                [ class "mb-3 mr-3" ]
                                (not model.scenarioIsRunning
                                    && not (ScenarioForm.hasCustomOriginHeaderWithKey "Set-Cookie" model.scenarioForm)
                                )
                                "Add Set-Cookie"
                                (AddOriginResponseHeaderWithKeyAndValue "Set-Cookie" "foo=bar")
                            , viewAddHeaderButton
                                [ class "mb-3" ]
                                (not model.scenarioIsRunning)
                                "Add custom"
                                AddCustomOriginResponseHeader
                            ]
                        ]
                    , div [ class "divider" ] []
                    , div
                        [ class "text-gray-700 space-y-4" ]
                        [ h2
                            [ class "font-medium text-gray-900 dark:text-gray-100" ]
                            [ text "Response code and body"
                            , span
                                [ class "font-normal text-gray-700" ]
                                [ text " for " ]
                            , span
                                [ class "font-mono text-gray-700" ]
                                [ text "/ids/:id" ]
                            ]
                        , p
                            []
                            [ text "By default, the response status code is 200 and the body is the current Unix time (in seconds)." ]
                        , div
                            [ Html.Attributes.class "form-control w-fit"
                            ]
                            [ label
                                [ Html.Attributes.class "label cursor-pointer pl-0"
                                ]
                                [ input
                                    [ Html.Attributes.type_ "checkbox"
                                    , Html.Attributes.disabled model.scenarioIsRunning
                                    , Html.Attributes.class "toggle"
                                    , Html.Attributes.checked <| ScenarioForm.originReturn304ForConditionalRequests model.scenarioForm
                                    , Html.Attributes.id "toggle-origin-toggle-origin-return-304-for-conditional-requests"
                                    , Html.Events.onClick ToggleOriginReturn304ForConditionalRequests
                                    ]
                                    []
                                , span
                                    [ Html.Attributes.class "label-text text-base ml-3"
                                    ]
                                    [ text "Return 304 and empty body for "
                                    , em [] [ text "all" ]
                                    , text " conditional requests"
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "divider" ] []
                    , div
                        [ class "space-y-4" ]
                        [ h2
                            [ class "font-medium text-gray-900 dark:text-gray-100" ]
                            [ text "Response time"
                            , span
                                [ class "font-normal text-gray-700" ]
                                [ text " for " ]
                            , span
                                [ class "font-mono text-gray-700" ]
                                [ text "/ids/:id" ]
                            ]
                        , div
                            [ Html.Attributes.class "form-control w-fit"
                            ]
                            [ label
                                [ Html.Attributes.class "label cursor-pointer pl-0"
                                ]
                                [ input
                                    [ Html.Attributes.type_ "checkbox"
                                    , Html.Attributes.disabled model.scenarioIsRunning
                                    , Html.Attributes.class "toggle"
                                    , Html.Attributes.checked <| ScenarioForm.originWait2SecondsBeforeResponding model.scenarioForm
                                    , Html.Attributes.id "toggle-origin-wait-2-seconds-before-responding"
                                    , Html.Events.onClick ToggleOriginWait2SecondsBeforeResponding
                                    ]
                                    []
                                , span
                                    [ Html.Attributes.class "label-text text-base ml-3"
                                    ]
                                    [ text "Wait 2 seconds before responding" ]
                                ]
                            ]
                        ]
                    ]
                ]
        ]


viewOriginSettingsForExercise : Model -> Html Msg
viewOriginSettingsForExercise model =
    let
        originHeaders : List ( String, String )
        originHeaders =
            model.scenarioForm
                |> ScenarioForm.originHeaders
                |> List.map
                    (\originHeader ->
                        case originHeader of
                            ScenarioForm.CacheControl cacheControlResponseDirectives ->
                                ( "Cache-Control", CacheControlResponseDirectives.toString True cacheControlResponseDirectives )

                            ScenarioForm.Custom header ->
                                ( header.key, header.value )
                    )

        settings : List (Html Msg)
        settings =
            ((originHeaders
                |> List.map
                    (\( key, value ) ->
                        span
                            [ class "font-mono" ]
                            [ text key
                            , text ": "
                            , text value
                            ]
                    )
             )
                ++ (if ScenarioForm.originReturn304ForConditionalRequests model.scenarioForm then
                        [ span []
                            [ text "Return a 304 and empty body for "
                            , em [] [ text "all" ]
                            , text " conditional requests."
                            ]
                        ]

                    else
                        []
                   )
            )
                |> List.map (\body -> li [] [ body ])
    in
    Extras.Html.showUnless (List.isEmpty settings) <|
        div
            [ class "mt-8" ]
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100 mb-8" ]
                [ text "Origin" ]
            , ul
                [ class "list-disc list-inside space-y-1" ]
                settings
            ]


view : Model -> Document Msg
view model =
    let
        scenario : Scenario
        scenario =
            ScenarioForm.toScenario model.scenarioForm (model.id |> Maybe.withDefault "")

        allRequestHeaderKeys : List String
        allRequestHeaderKeys =
            Scenario.allRequestHeaderKeys scenario

        allResponseHeaderKeys : List String
        allResponseHeaderKeys =
            Scenario.allResponseHeaderKeys scenario

        userShouldSeeExerciseForm : Bool
        userShouldSeeExerciseForm =
            not (model.interactions |> Result.map Interactions.isEmpty |> Result.withDefault True)
                && not model.scenarioIsRunning
                && (model.sequenceDiagramVisibility
                        == FinalInteractionsConcealedForExercise
                        || model.sequenceDiagramVisibility
                        == FinalInteractionsRevealedForExercise
                   )

        pageTitle : String -> String
        pageTitle =
            model.scenarioForm
                |> ScenarioForm.exerciseTitle
                |> Maybe.map (\exerciseTitle overallTitle -> exerciseTitle ++ " · " ++ overallTitle)
                |> Maybe.withDefault identity
    in
    { title = pageTitle "Web Cache Playground"
    , body =
        [ div
            [ class "min-h-full pt-4 pb-10 sm:pt-6 md:pt-10" ]
            [ header []
                [ div
                    [ class "mx-auto max-w-screen-2xl px-4 sm:px-6 lg:px-8" ]
                    [ div
                        [ class "sm:flex sm:items-center sm:justify-between" ]
                        [ div
                            [ class "min-w-0 flex-1" ]
                            [ h1
                                [ class "text-3xl font-bold leading-tight tracking-tight text-gray-900" ]
                                [ text "Web Cache Playground" ]
                            ]
                        , div
                            [ class "mt-4 flex sm:ml-4 sm:mt-0" ]
                            [ a
                                [ href "https://github.com/hilverd/web-cache-playground"
                                , Html.Attributes.target "_blank"
                                , Html.Attributes.rel "noopener noreferrer"
                                , class "mb-6 sm:mb-0 block text-slate-400 dark:text-slate-200 hover:text-slate-500 dark:hover:text-slate-400"
                                ]
                                [ span
                                    [ class "sr-only" ]
                                    [ text "Web Cache Playground on GitHub" ]
                                , Icons.gitHubLogo
                                    [ Svg.Attributes.class "h-6 w-6" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , main_
                [ class "mx-auto max-w-screen-2xl px-4 sm:px-6 lg:px-8" ]
                [ viewScenarioForm model
                , Extras.Html.showUnless
                    (model.sequenceDiagramVisibility == FinalInteractionsConcealedForExercise || model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise)
                  <|
                    div
                        [ class "inline-flex items-center mt-8" ]
                        [ button
                            [ class "btn btn-primary mr-4"
                            , Html.Events.onClick RunScenarioFromForm
                            , Html.Attributes.disabled <|
                                (model.scenarioIsRunning
                                    || (model.scenarioForm |> ScenarioForm.clientActions |> Array.isEmpty)
                                )
                            ]
                            [ Icons.play [ Svg.Attributes.class "h-5 w-5" ]
                            , text "Run scenario"
                            ]
                        , button
                            [ class "btn btn-warning"
                            , Html.Events.onClick ResetScenarioForm
                            , Html.Attributes.disabled <| (model.scenarioIsRunning || ScenarioForm.isEmpty model.scenarioForm)
                            ]
                            [ text "Reset form" ]
                        ]
                , h2
                    [ class "mt-8 text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                    [ text "Interactions" ]
                , model.interactions
                    |> Result.map
                        (\interactions ->
                            let
                                interactionsToShow =
                                    if model.sequenceDiagramVisibility == CompletelyRevealed || model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise then
                                        interactions

                                    else
                                        Interactions.withoutInteractionsAfterFinalClientAction
                                            (Scenario.length scenario - 1)
                                            interactions
                            in
                            div
                                [ Extras.HtmlAttribute.showIf model.formWasModifiedSinceScenarioRun <| class "opacity-50"
                                ]
                                [ Extras.Html.showUnless (Interactions.isEmpty interactionsToShow || (ScenarioForm.exerciseTitle model.scenarioForm /= Nothing)) <|
                                    Extras.Html.showMaybe
                                        (\id ->
                                            div
                                                []
                                                [ div
                                                    [ class "mt-8" ]
                                                    [ div
                                                        [ Html.Attributes.class "form-control w-fit"
                                                        ]
                                                        [ label
                                                            [ Html.Attributes.class "label cursor-pointer"
                                                            ]
                                                            [ input
                                                                [ Html.Attributes.type_ "checkbox"
                                                                , Html.Attributes.class "toggle"
                                                                , Html.Attributes.checked model.showAllHeaders
                                                                , Html.Attributes.id "toggle-show-all-headers"
                                                                , Html.Events.onClick ToggleShowAllHeaders
                                                                ]
                                                                []
                                                            , span
                                                                [ Html.Attributes.class "label-text text-base ml-3"
                                                                ]
                                                                [ text "Show all headers" ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                        )
                                        model.id
                                , Interactions.view
                                    { scenarioIsRunning = model.scenarioIsRunning
                                    , showAllHeaders = model.showAllHeaders
                                    , allRequestHeaderKeys = allRequestHeaderKeys
                                    , allResponseHeaderKeys = allResponseHeaderKeys
                                    , sequenceDiagramVisibility = model.sequenceDiagramVisibility
                                    }
                                    interactionsToShow
                                ]
                        )
                    |> Result.withDefault
                        (p
                            [ class "mt-4 text-red-700" ]
                            [ text "Error while retrieving interaction log." ]
                        )
                , Extras.Html.showIf userShouldSeeExerciseForm <|
                    div
                        [ class "mt-6 flex flex-col justify-center items-center" ]
                        [ div
                            [ class "text-gray-700 font-medium text-lg" ]
                            [ text "What do you think happens next?" ]
                        , Extras.Html.showMaybe
                            (\exerciseAnswers ->
                                div
                                    [ class "mt-4" ]
                                    (exerciseAnswers
                                        |> Array.toList
                                        |> List.indexedMap
                                            (\index exerciseAnswer ->
                                                div
                                                    [ class "form-control" ]
                                                    [ label
                                                        [ class "label cursor-pointer" ]
                                                        [ span
                                                            [ class "label-text inline-flex items-center text-lg max-w-prose" ]
                                                            [ Extras.Html.showIf (model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise) <|
                                                                span
                                                                    []
                                                                    [ if exerciseAnswer.correct then
                                                                        Icons.circleCheck
                                                                            [ Svg.Attributes.class "h-7 w-7 text-green-800" ]

                                                                      else
                                                                        Icons.circleX
                                                                            [ Svg.Attributes.class "h-7 w-7 text-red-800" ]
                                                                    ]
                                                            , Extras.Html.showIf (model.sequenceDiagramVisibility == FinalInteractionsConcealedForExercise) <|
                                                                span
                                                                    []
                                                                    [ Icons.circleX
                                                                        -- this is just a spacer
                                                                        [ Svg.Attributes.class "h-7 w-7 text-white" ]
                                                                    ]
                                                            , input
                                                                [ type_ "radio"
                                                                , name "exercise-answer"
                                                                , class "ml-3 radio mr-3"
                                                                , Html.Events.onClick <| SelectExerciseAnswer index
                                                                , checked exerciseAnswer.selected
                                                                , disabled <| model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise
                                                                ]
                                                                []
                                                            , span
                                                                []
                                                                [ text exerciseAnswer.answer
                                                                , Extras.Html.showIf (model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise && exerciseAnswer.selected && exerciseAnswer.correct) <|
                                                                    span
                                                                        [ class "ml-4 text-green-800" ]
                                                                        [ text "Correct!" ]
                                                                , Extras.Html.showIf (model.sequenceDiagramVisibility == FinalInteractionsRevealedForExercise && exerciseAnswer.selected && not exerciseAnswer.correct) <|
                                                                    span
                                                                        [ class "ml-4 text-red-800" ]
                                                                        [ text "Incorrect!" ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                            )
                                    )
                            )
                            (ScenarioForm.exerciseAnswers model.scenarioForm)
                        , div
                            [ class "mt-4 inline-flex items-center" ]
                            [ button
                                [ class "btn btn-lg btn-primary"
                                , disabled <|
                                    ((not <| ScenarioForm.someExerciseAnswerIsSelected model.scenarioForm)
                                        || model.sequenceDiagramVisibility
                                        /= FinalInteractionsConcealedForExercise
                                    )
                                , Html.Events.onClick SubmitExerciseForm
                                ]
                                [ text "Submit" ]
                            , button
                                [ class "ml-3 btn btn-warning btn-lg"
                                , Html.Events.onClick LeaveExercise
                                ]
                                [ text "Leave exercise" ]
                            ]
                        ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
