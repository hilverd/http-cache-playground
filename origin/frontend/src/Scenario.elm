module Scenario exposing (Action(..), Scenario, actions, create, id, isEmpty, nextAction)


type Action
    = SleepForSeconds Int
    | MakeGetRequest
        { path : String
        , headers : List ( String, String )
        , desiredResponseHeaders : List ( String, String )
        , respondSlowly : Bool
        }


type Scenario
    = Scenario { id_ : String, actions_ : List Action }


create : String -> List Action -> Scenario
create id_ actions_ =
    Scenario { id_ = id_, actions_ = actions_ }


isEmpty : Scenario -> Bool
isEmpty (Scenario { actions_ }) =
    List.isEmpty actions_


id : Scenario -> String
id (Scenario { id_ }) =
    id_


actions : Scenario -> List Action
actions (Scenario { actions_ }) =
    actions_


nextAction : Scenario -> Maybe ( Action, Scenario )
nextAction (Scenario { id_, actions_ }) =
    case actions_ of
        head :: tail ->
            Just ( head, Scenario { id_ = id_, actions_ = tail } )

        _ ->
            Nothing
