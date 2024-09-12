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
import Dict
import ElementIds
import Extras.BrowserDom
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html exposing (..)
import Html.Attributes exposing (checked, class, disabled, href, id, type_)
import Html.Events
import Http
import Icons
import Interaction
import Interactions exposing (Interactions)
import Process
import Random
import Regex
import Scenario exposing (Scenario)
import ScenarioForm exposing (ScenarioForm, clientActions, originWait2SecondsBeforeResponding)
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
    }


scenarioFromForm : ScenarioForm -> String -> Scenario
scenarioFromForm form id =
    let
        path : String
        path =
            "/from-browser/ids/" ++ id

        clientActions : Array.Array ScenarioForm.ClientAction
        clientActions =
            ScenarioForm.clientActions form

        originWait2SecondsBeforeResponding : Bool
        originWait2SecondsBeforeResponding =
            ScenarioForm.originWait2SecondsBeforeResponding form

        desiredResponseHeaders : List ( String, String )
        desiredResponseHeaders =
            form
                |> ScenarioForm.originHeadersAsPairs
                |> List.filter (\( key, value ) -> key /= "" && value /= "")
    in
    clientActions
        |> Array.toList
        |> List.map
            (\clientAction ->
                case clientAction of
                    ScenarioForm.MakeGetRequest headers ->
                        Scenario.MakeGetRequest
                            { path = path
                            , headers =
                                headers
                                    |> Array.toList
                                    |> List.map (\{ key, value } -> ( key, value ))
                                    |> List.filter (\( key, value ) -> key /= "" && value /= "")
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = originWait2SecondsBeforeResponding
                            }

                    ScenarioForm.SleepForOneSecond ->
                        Scenario.SleepForSeconds 1

                    ScenarioForm.SleepForTwoSeconds ->
                        Scenario.SleepForSeconds 2

                    ScenarioForm.SleepForThreeSeconds ->
                        Scenario.SleepForSeconds 3

                    ScenarioForm.SleepForFiveSeconds ->
                        Scenario.SleepForSeconds 5

                    ScenarioForm.SleepForEightSeconds ->
                        Scenario.SleepForSeconds 8
            )
        |> Scenario.create id


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , id = Nothing
      , scenarioForm = ScenarioForm.fromUrl url
      , scenarioIsRunning = False
      , interactions = Ok Interactions.empty
      , formWasModifiedSinceScenarioRun = False
      , showAllHeaders = False
      }
    , Cmd.none
    )



-- PORTS


port scrollToBottomOfSequenceDiagram : () -> Cmd msg



-- UPDATE


type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | MakeUrlReflectScenarioForm ScenarioForm
    | AddMakeGetRequest
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
    | AddCustomOriginResponseHeader
    | AddOriginResponseHeaderWithKey String
    | AddOriginResponseHeaderWithKeyAndValue String String
    | AddOriginCacheControlHeader
    | ChangeMaxAge Int (Maybe Int)
    | ChangeSMaxAge Int (Maybe Int)
    | ToggleNoStore Int
    | TogglePrivate Int
    | ChangeStaleWhileRevalidate Int (Maybe Int)
    | DeleteCustomOriginResponseHeader Int
    | UpdateCustomOriginResponseHeaderKey Int String
    | UpdateCustomOriginResponseHeaderValue Int String
    | RunScenarioFromForm
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
        }
        (Result Http.Error ())
    | GotResponseToGetRequest Scenario (Result Http.Error ( Http.Metadata, String ))
    | RecordedResponseToGetRequest Scenario (Result Http.Error ())
    | ToggleShowAllHeaders


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
            in
            ( { model | scenarioForm = scenarioForm }
            , Cmd.none
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

        NewUuid uuid ->
            let
                scenario : Scenario
                scenario =
                    scenarioFromForm model.scenarioForm uuid
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

        RecordedMakeGetRequest restOfScenario { path, headers, desiredResponseHeaders, respondSlowly } _ ->
            ( model
            , Cmd.batch
                [ getInteractions <| Scenario.id restOfScenario
                , makeGetRequest restOfScenario
                    { path = path
                    , headers = headers
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
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

        ResetScenarioForm ->
            updateScenarioForm
                (always ScenarioForm.empty)
                [ Extras.BrowserDom.scrollToTop NoOp ]
                model

        ToggleShowAllHeaders ->
            ( { model | showAllHeaders = not model.showAllHeaders }
            , Cmd.none
            )


updateScenarioForm : (ScenarioForm -> ScenarioForm) -> List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
updateScenarioForm f commands model =
    let
        updatedScenarioForm =
            f model.scenarioForm
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


getInteractions : String -> Cmd Msg
getInteractions id =
    let
        url =
            Url.Builder.crossOrigin
                "http://localhost:8080"
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
            (\( action, restOfScenario ) ->
                case action of
                    Scenario.SleepForSeconds seconds ->
                        recordSleepForSeconds restOfScenario seconds

                    Scenario.MakeGetRequest { path, headers, desiredResponseHeaders, respondSlowly } ->
                        recordMakeGetRequest restOfScenario
                            { path = path
                            , headers = headers
                            , desiredResponseHeaders = desiredResponseHeaders
                            , respondSlowly = respondSlowly
                            }
            )
        |> Maybe.withDefault
            (Process.sleep 2500
                |> Task.perform (always GetInteractionsAfterScenarioHasFinished)
            )


recordSleepForSeconds : Scenario -> Int -> Cmd Msg
recordSleepForSeconds restOfScenario seconds =
    let
        id =
            Scenario.id restOfScenario

        url =
            Url.Builder.crossOrigin
                "http://localhost:8080"
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.ClientSleepingForSeconds seconds
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect = Http.expectWhatever <| RecordedSleepForSeconds restOfScenario seconds
        }


recordMakeGetRequest :
    Scenario
    ->
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        }
    -> Cmd Msg
recordMakeGetRequest restOfScenario { path, headers, desiredResponseHeaders, respondSlowly } =
    let
        id =
            Scenario.id restOfScenario

        url =
            Url.Builder.crossOrigin
                "http://localhost:8080"
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.ClientToVarnish
                { path = path, headers = headers }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect =
            Http.expectWhatever <|
                RecordedMakeGetRequest restOfScenario
                    { path = path
                    , headers = headers
                    , desiredResponseHeaders = desiredResponseHeaders
                    , respondSlowly = respondSlowly
                    }
        }


makeGetRequest :
    Scenario
    ->
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        }
    -> Cmd Msg
makeGetRequest restOfScenario { path, headers, desiredResponseHeaders, respondSlowly } =
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
            Url.Builder.crossOrigin
                "http://localhost:8080"
                [ pathWithoutLeadingSlash ]
                (queryParametersForHeadersToSend
                    ++ queryParametersForHeadersToReturn
                    ++ (if respondSlowly then
                            [ Url.Builder.string "respond-slowly" "" ]

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
        { method = "GET"
        , headers = headerToAvoidBrowserCaching :: httpHeaders
        , url = url
        , body = Http.emptyBody
        , expect = expectWhateverResponse <| GotResponseToGetRequest restOfScenario
        , timeout = Nothing
        , tracker = Nothing
        }


recordResponseToGetRequest : Scenario -> Http.Metadata -> String -> Cmd Msg
recordResponseToGetRequest restOfScenario metadata responseBody =
    let
        id =
            Scenario.id restOfScenario

        url =
            Url.Builder.crossOrigin
                "http://localhost:8080"
                [ "interactions", id, "new" ]
                []
    in
    Http.post
        { url = url
        , body =
            Interaction.VarnishToClient
                { statusCode = metadata.statusCode
                , headers = metadata.headers |> Dict.toList
                , body = responseBody
                }
                |> Codec.encodeToValue Interaction.codec
                |> Http.jsonBody
        , expect = Http.expectWhatever <| RecordedResponseToGetRequest restOfScenario
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
viewGetRequestHeader scenarioIsRunning stepIndex index header =
    let
        keyIsForbidden =
            isForbiddenHeaderName header.key
    in
    div
        [ class "flex-auto max-w-2xl flex" ]
        [ button
            [ class "btn btn-circle mr-3"
            , Accessibility.Aria.label "Delete"
            , Html.Attributes.disabled scenarioIsRunning
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
                    , Html.Attributes.disabled scenarioIsRunning
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
                    , Html.Attributes.disabled scenarioIsRunning
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
viewOriginHeader scenarioIsRunning index originHeader =
    div
        [ class "flex-auto max-w-2xl flex items-center" ]
        [ button
            [ class "btn btn-circle mr-3"
            , Accessibility.Aria.label "Delete"
            , Html.Attributes.disabled scenarioIsRunning
            , Html.Events.onClick <| DeleteCustomOriginResponseHeader index
            ]
            [ Icons.trash
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        , case originHeader of
            ScenarioForm.CacheControl cacheControlResponseDirectives ->
                viewOriginCacheControlHeader scenarioIsRunning index cacheControlResponseDirectives

            ScenarioForm.Custom header ->
                viewOriginCustomHeader scenarioIsRunning index header
        ]


viewOriginCustomHeader : Bool -> Int -> ScenarioForm.Header -> Html Msg
viewOriginCustomHeader scenarioIsRunning index header =
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
                , Html.Attributes.disabled scenarioIsRunning
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
                , Html.Attributes.disabled scenarioIsRunning
                , Html.Attributes.id <| ElementIds.originCustomHeaderValue index
                , Accessibility.Aria.required True
                , Accessibility.Aria.label "Value"
                , Html.Attributes.value header.value
                , Html.Events.onInput (UpdateCustomOriginResponseHeaderValue index)
                , Extras.HtmlEvents.onEnter NoOp
                ]
                []
            ]
        ]


viewOriginCacheControlHeader : Bool -> Int -> CacheControlResponseDirectives -> Html Msg
viewOriginCacheControlHeader scenarioIsRunning index directives =
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

        headerValue =
            CacheControlResponseDirectives.toString directives
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
                [ viewOriginCacheControlMaxAgeDirective scenarioIsRunning index maxAge
                , viewOriginCacheControlSMaxAgeDirective scenarioIsRunning index sMaxAge
                , viewOriginCacheControlNoStoreDirective scenarioIsRunning index noStore
                , viewOriginCacheControlPrivateDirective scenarioIsRunning index private
                , viewOriginCacheControlStaleWhileRevalidateDirective scenarioIsRunning index staleWhileRevalidate
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


viewOriginCacheControlMaxAgeDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlMaxAgeDirective scenarioIsRunning index maxAgeSeconds =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| maxAgeSeconds /= Nothing
            , class "checkbox"
            , disabled scenarioIsRunning
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
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| class "cursor-pointer"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <|
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
            (not scenarioIsRunning)
            [ 0, 1, 2, 3, 5 ]
            (ChangeMaxAge index << Just)
            (maxAgeSeconds |> Maybe.withDefault 0)
        ]


viewOriginCacheControlSMaxAgeDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlSMaxAgeDirective scenarioIsRunning index sMaxAgeSeconds =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| sMaxAgeSeconds /= Nothing
            , class "checkbox"
            , disabled scenarioIsRunning
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
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| class "cursor-pointer"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <|
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
            (not scenarioIsRunning)
            [ 0, 1, 2, 3, 5 ]
            (ChangeSMaxAge index << Just)
            (sMaxAgeSeconds |> Maybe.withDefault 0)
        ]


viewOriginCacheControlNoStoreDirective : Bool -> Int -> Bool -> Html Msg
viewOriginCacheControlNoStoreDirective scenarioIsRunning index noStore =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked noStore
            , class "checkbox"
            , disabled scenarioIsRunning
            , Html.Attributes.id <| ElementIds.originCacheControlNoStore index
            , Html.Events.onClick <| ToggleNoStore index
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| class "cursor-pointer"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| Html.Events.onClick <| ToggleNoStore index
            ]
            [ span
                [ class "font-mono" ]
                [ text "no-store" ]
            ]
        ]


viewOriginCacheControlPrivateDirective : Bool -> Int -> Bool -> Html Msg
viewOriginCacheControlPrivateDirective scenarioIsRunning index private =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked private
            , class "checkbox"
            , disabled scenarioIsRunning
            , Html.Attributes.id <| ElementIds.originCacheControlPrivate index
            , Html.Events.onClick <| TogglePrivate index
            ]
            []
        , div
            [ class "ml-2 flex flex-col select-none"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| class "cursor-pointer"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| Html.Events.onClick <| TogglePrivate index
            ]
            [ span
                [ class "font-mono" ]
                [ text "private" ]
            ]
        ]


viewOriginCacheControlStaleWhileRevalidateDirective : Bool -> Int -> Maybe Int -> Html Msg
viewOriginCacheControlStaleWhileRevalidateDirective scenarioIsRunning index staleWhileRevalidate =
    div
        [ class "px-1 pt-3 mr-4 text-gray-800 flex" ]
        [ input
            [ type_ "checkbox"
            , checked <| staleWhileRevalidate /= Nothing
            , class "checkbox"
            , disabled scenarioIsRunning
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
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <| class "cursor-pointer"
            , Extras.HtmlAttribute.showUnless scenarioIsRunning <|
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
            (not scenarioIsRunning)
            [ 0, 1, 2, 3, 5 ]
            (ChangeStaleWhileRevalidate index << Just)
            (staleWhileRevalidate |> Maybe.withDefault 0)
        ]


viewClientAction : Bool -> Int -> ScenarioForm.ClientAction -> Html Msg
viewClientAction scenarioIsRunning stepIndex clientAction =
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
                        [ class "ml-2 font-mono" ]
                        [ text "GET /ids/:id" ]
                    ]
                , div
                    [ class "px-4 mt-6" ]
                    [ h4
                        [ class "font-medium text-gray-700" ]
                        [ text "Request headers" ]
                    ]
                , div
                    [ class "space-y-4 px-4 mt-4 max-w-full" ]
                    (headers
                        |> Array.toList
                        |> List.indexedMap (viewGetRequestHeader scenarioIsRunning stepIndex)
                    )
                , div
                    [ class "ml-4 mt-6" ]
                    [ viewAddHeaderButton
                        [ class "mb-3 mr-3" ]
                        (not scenarioIsRunning
                            && not (ScenarioForm.clientActionHasGetRequestHeaderWithKey "If-Match" clientAction)
                        )
                        "Add If-Match"
                        (AddGetRequestHeaderWithKey stepIndex "If-Match")
                    , viewAddHeaderButton
                        [ class "mb-3 mr-3" ]
                        (not scenarioIsRunning
                            && not (ScenarioForm.clientActionHasGetRequestHeaderWithKey "If-None-Match" clientAction)
                        )
                        "Add If-None-Match"
                        (AddGetRequestHeaderWithKeyAndValue stepIndex "If-None-Match" "\"some-etag\"")
                    , viewAddHeaderButton
                        [ class "mb-3 mr-3" ]
                        (not scenarioIsRunning
                            && not (ScenarioForm.clientActionHasGetRequestHeaderWithKey "If-Modified-Since" clientAction)
                        )
                        "Add If-Modified-Since"
                        (AddGetRequestHeaderWithKeyAndValue stepIndex "If-Modified-Since" "Wed, 21 Oct 2015 07:28:00 GMT")
                    , viewAddHeaderButton
                        []
                        (not scenarioIsRunning)
                        "Add custom"
                        (AddGetRequestHeader stepIndex)
                    ]
                , div
                    [ class "pr-3 pb-2" ]
                    [ span
                        [ class "flex justify-end group" ]
                        [ Components.Button.text
                            (not scenarioIsRunning)
                            [ Accessibility.Key.tabbable <| not scenarioIsRunning
                            , Html.Events.onClick <| DeleteClientAction stepIndex
                            ]
                            [ Icons.trash
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300"
                                , if scenarioIsRunning then
                                    Svg.Attributes.class ""

                                  else
                                    Svg.Attributes.class "group-hover:text-gray-500 dark:group-hover:text-gray-400"
                                ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300"
                                , if scenarioIsRunning then
                                    class ""

                                  else
                                    class "group-hover:text-gray-700 dark:group-hover:text-gray-400"
                                ]
                                [ text "Delete" ]
                            ]
                        ]
                    ]
                ]

        ScenarioForm.SleepForOneSecond ->
            viewSleepForSeconds scenarioIsRunning stepIndex 1

        ScenarioForm.SleepForTwoSeconds ->
            viewSleepForSeconds scenarioIsRunning stepIndex 2

        ScenarioForm.SleepForThreeSeconds ->
            viewSleepForSeconds scenarioIsRunning stepIndex 3

        ScenarioForm.SleepForFiveSeconds ->
            viewSleepForSeconds scenarioIsRunning stepIndex 5

        ScenarioForm.SleepForEightSeconds ->
            viewSleepForSeconds scenarioIsRunning stepIndex 8


viewSleepForSeconds : Bool -> Int -> Int -> Html Msg
viewSleepForSeconds scenarioIsRunning stepIndex seconds =
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
                , Extras.HtmlAttribute.showIf (seconds == 1) <| class "mr-1.5"
                ]
                [ text "Sleep "
                , text <| String.fromInt seconds
                , text
                    (if seconds == 1 then
                        " second"

                     else
                        " seconds"
                    )
                ]
            , Components.RangeSlider.viewSeconds
                (not scenarioIsRunning)
                [ 1, 2, 3, 5, 8 ]
                (ChangeSleepDuration stepIndex)
                seconds
            ]
        , div
            [ class "pr-3 pb-2" ]
            [ span
                [ class "flex justify-end group" ]
                [ Components.Button.text
                    (not scenarioIsRunning)
                    [ Accessibility.Key.tabbable <| not scenarioIsRunning
                    , Html.Events.onClick <| DeleteClientAction stepIndex
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300"
                        , if scenarioIsRunning then
                            Svg.Attributes.class ""

                          else
                            Svg.Attributes.class "group-hover:text-gray-500 dark:group-hover:text-gray-400"
                        ]
                    , span
                        [ class "font-medium text-gray-600 dark:text-gray-300"
                        , if scenarioIsRunning then
                            class ""

                          else
                            class "group-hover:text-gray-700 dark:group-hover:text-gray-400"
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]


viewScenarioForm : Model -> Html Msg
viewScenarioForm model =
    div []
        [ p
            [ class "mt-8 text-gray-600" ]
            [ text "Prepare a scenario for a single unique, randomly generated URL "
            , span
                [ class "font-mono" ]
                [ text "/ids/:id" ]
            , text " — then run it."
            ]
        , div
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
                        [ class "mt-4 text-gray-600" ]
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
                                viewClientAction model.scenarioIsRunning index action
                            )
                    )
                , div
                    []
                    [ button
                        [ class "btn mr-4 mb-4"
                        , Html.Attributes.disabled <|
                            (model.scenarioIsRunning
                                || ScenarioForm.hasTenClientActions model.scenarioForm
                            )
                        , Html.Events.onClick AddMakeGetRequest
                        ]
                        [ Icons.plus [ Svg.Attributes.class "h-5 w-5" ]
                        , text "Add GET request step"
                        ]
                    , button
                        [ class "btn"
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
                    ]
                ]
            , div
                [ class "space-y-4 mt-4 lg:mt-0" ]
                [ h2
                    [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100 mb-8" ]
                    [ text "Origin" ]
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
                            [ Html.Attributes.class "label cursor-pointer"
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
                , div [ class "divider" ] []
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
                    , Extras.Html.showIf
                        (model.scenarioForm
                            |> ScenarioForm.originHeaders
                            |> List.isEmpty
                        )
                      <|
                        p
                            [ class "mt-4 text-gray-700" ]
                            [ text "[No headers defined yet.]" ]
                    , div
                        [ class "space-y-4" ]
                        (model.scenarioForm
                            |> ScenarioForm.originHeaders
                            |> List.indexedMap (viewOriginHeader model.scenarioIsRunning)
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
                                && not (ScenarioForm.hasCustomOriginHeaderWithKey "Set-Cookie" model.scenarioForm)
                            )
                            "Add Set-Cookie"
                            (AddOriginResponseHeaderWithKeyAndValue "Set-Cookie" "foo=bar")
                        , viewAddHeaderButton
                            [ class "mb-3 mr-3" ]
                            (not model.scenarioIsRunning
                                && not (ScenarioForm.hasCustomOriginHeaderWithKey "Vary" model.scenarioForm)
                            )
                            "Add Vary"
                            (AddOriginResponseHeaderWithKeyAndValue "Vary" "Accept-Encoding")
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
                        [ text "Response body"
                        , span
                            [ class "font-normal text-gray-700" ]
                            [ text " for " ]
                        , span
                            [ class "font-mono text-gray-700" ]
                            [ text "/ids/:id" ]
                        ]
                    , p
                        []
                        [ text "The body is always the current Unix time (in seconds)." ]
                    ]
                ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "Varnish Cache Playground"
    , body =
        [ div
            [ class "min-h-full py-10" ]
            [ header []
                [ div
                    [ class "mx-auto max-w-screen-2xl px-4 sm:px-6 lg:px-8" ]
                    [ div
                        [ class "sm:flex sm:items-center sm:justify-between" ]
                        [ div
                            [ class "min-w-0 flex-1" ]
                            [ h1
                                [ class "text-3xl font-bold leading-tight tracking-tight text-gray-900" ]
                                [ text "Varnish Cache Playground" ]
                            ]
                        , div
                            [ class "mt-4 flex sm:ml-4 sm:mt-0" ]
                            [ a
                                [ href "https://github.com/hilverd/varnish-cache-playground"
                                , Html.Attributes.target "_blank"
                                , Html.Attributes.rel "noopener noreferrer"
                                , class "mb-6 sm:mb-0 block text-slate-400 dark:text-slate-200 hover:text-slate-500 dark:hover:text-slate-400"
                                ]
                                [ span
                                    [ class "sr-only" ]
                                    [ text "Varnish Cache Playground on GitHub" ]
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
                , div
                    [ class "inline-flex items-center mt-8" ]
                    [ button
                        [ class "btn btn-primary"
                        , Html.Events.onClick RunScenarioFromForm
                        , Html.Attributes.disabled <|
                            (model.scenarioIsRunning
                                || (model.scenarioForm |> ScenarioForm.clientActions |> Array.isEmpty)
                            )
                        ]
                        [ Icons.play [ Svg.Attributes.class "h-5 w-5" ]
                        , text "Run scenario"
                        ]
                    , span
                        [ class "ml-4" ]
                        [ button
                            [ class "btn btn-warning"
                            , Html.Events.onClick ResetScenarioForm
                            , Html.Attributes.disabled <| (model.scenarioIsRunning || ScenarioForm.isEmpty model.scenarioForm)
                            ]
                            [ text "Reset form"
                            ]
                        ]
                    ]
                , h2
                    [ class "mt-8 text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                    [ text "Interactions" ]
                , model.interactions
                    |> Result.map
                        (\interactions ->
                            div
                                [ Extras.HtmlAttribute.showIf model.formWasModifiedSinceScenarioRun <| class "opacity-50"
                                ]
                                [ Extras.Html.showUnless (Interactions.isEmpty interactions) <|
                                    Extras.Html.showMaybe
                                        (\id ->
                                            div
                                                []
                                                [ p
                                                    [ class "mt-4 text-gray-700" ]
                                                    [ text <| "… for randomly generated ID "
                                                    , span
                                                        [ class "font-mono" ]
                                                        [ text id ]
                                                    , text "."
                                                    ]
                                                , div
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
                                    }
                                    interactions
                                ]
                        )
                    |> Result.withDefault
                        (p
                            [ class "mt-4 text-red-700" ]
                            [ text "Error while retrieving interaction log." ]
                        )
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
