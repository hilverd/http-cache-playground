module Scenario exposing (Action(..), Scenario, actions, allRequestHeaderKeys, allResponseHeaderKeys, create, id, isEmpty, length, nextAction)

import Set


type Action
    = SleepForSeconds Int
    | MakeGetRequest
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        , auto304 : Bool
        }
    | MakePurgeRequest { path : String }


type Scenario
    = Scenario
        { id_ : String
        , actions_ : List ( Int, Action )
        }


create : String -> List ( Int, Action ) -> Scenario
create id_ actions_ =
    Scenario { id_ = id_, actions_ = actions_ }


isEmpty : Scenario -> Bool
isEmpty (Scenario { actions_ }) =
    List.isEmpty actions_


id : Scenario -> String
id (Scenario { id_ }) =
    id_


actions : Scenario -> List ( Int, Action )
actions (Scenario { actions_ }) =
    actions_


length : Scenario -> Int
length (Scenario { actions_ }) =
    List.length actions_


nextAction : Scenario -> Maybe ( ( Int, Action ), Scenario )
nextAction (Scenario { id_, actions_ }) =
    case actions_ of
        head :: tail ->
            Just ( head, Scenario { id_ = id_, actions_ = tail } )

        _ ->
            Nothing


allRequestHeaderKeys : Scenario -> List String
allRequestHeaderKeys (Scenario { actions_ }) =
    actions_
        |> List.map
            (\( _, action ) ->
                case action of
                    MakeGetRequest { headers } ->
                        headers
                            |> List.map (String.toLower << Tuple.first)
                            |> List.filter ((/=) "")

                    _ ->
                        []
            )
        |> List.concat
        |> Set.fromList
        |> Set.toList


allResponseHeaderKeys : Scenario -> List String
allResponseHeaderKeys (Scenario { actions_ }) =
    actions_
        |> List.map
            (\( _, action ) ->
                case action of
                    MakeGetRequest { desiredResponseHeaders } ->
                        desiredResponseHeaders
                            |> List.map (String.toLower << Tuple.first)
                            |> List.filter ((/=) "")

                    _ ->
                        []
            )
        |> List.concat
        |> Set.fromList
        |> Set.toList
