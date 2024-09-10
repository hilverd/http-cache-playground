module Interaction exposing (Interaction(..), codec)

import Codec exposing (Codec)


type Interaction
    = ClientSleepingForSeconds Int
    | ClientToVarnish
        { path : String
        , headers : List ( String, String )
        }
    | VarnishToOrigin
        { path : String
        , headers : List ( String, String )
        }
    | OriginSleepingForSeconds Int
    | OriginToVarnish
        { statusCode : Int
        , headers : List ( String, String )
        , body : String
        }
    | VarnishToClient
        { statusCode : Int
        , headers : List ( String, String )
        , body : String
        }


codec : Codec Interaction
codec =
    Codec.custom
        (\clientSleepingForSeconds clientToVarnish varnishToOrigin originSleepingForSeconds originToVarnish varnishToClient value ->
            case value of
                ClientSleepingForSeconds seconds ->
                    clientSleepingForSeconds seconds

                ClientToVarnish request ->
                    clientToVarnish request

                VarnishToOrigin request ->
                    varnishToOrigin request

                OriginSleepingForSeconds seconds ->
                    originSleepingForSeconds seconds

                OriginToVarnish response ->
                    originToVarnish response

                VarnishToClient response ->
                    varnishToClient response
        )
        |> Codec.variant1 "ClientSleepingForSeconds" ClientSleepingForSeconds Codec.int
        |> Codec.variant1 "ClientToVarnish"
            ClientToVarnish
            (Codec.object (\path headers -> { path = path, headers = headers })
                |> Codec.field "path" .path Codec.string
                |> Codec.field "headers"
                    .headers
                    (Codec.list <| Codec.tuple Codec.string Codec.string)
                |> Codec.buildObject
            )
        |> Codec.variant1 "VarnishToOrigin"
            VarnishToOrigin
            (Codec.object (\path headers -> { path = path, headers = headers })
                |> Codec.field "path" .path Codec.string
                |> Codec.field "headers"
                    .headers
                    (Codec.list <| Codec.tuple Codec.string Codec.string)
                |> Codec.buildObject
            )
        |> Codec.variant1 "OriginSleepingForSeconds" OriginSleepingForSeconds Codec.int
        |> Codec.variant1 "OriginToVarnish"
            OriginToVarnish
            (Codec.object (\statusCode headers body -> { statusCode = statusCode, headers = headers, body = body })
                |> Codec.field "statusCode" .statusCode Codec.int
                |> Codec.field "headers"
                    .headers
                    (Codec.list <| Codec.tuple Codec.string Codec.string)
                |> Codec.field "body" .body Codec.string
                |> Codec.buildObject
            )
        |> Codec.variant1 "VarnishToClient"
            VarnishToClient
            (Codec.object (\statusCode headers body -> { statusCode = statusCode, headers = headers, body = body })
                |> Codec.field "statusCode" .statusCode Codec.int
                |> Codec.field "headers"
                    .headers
                    (Codec.list <| Codec.tuple Codec.string Codec.string)
                |> Codec.field "body" .body Codec.string
                |> Codec.buildObject
            )
        |> Codec.buildCustom
