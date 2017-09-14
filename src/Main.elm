module Main exposing (..)

import Set exposing (Set)
import List.Extra as List
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra as HE


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { teams : List Team
    , matches : List Match
    , newTeamInput : String
    }


type Msg
    = NoOp
    | SetNewTeamInput String
    | AddTeam Team
    | DeleteTeam Team


type alias Team =
    String


type alias Match =
    ( ( Team, Score ), ( Team, Score ) )


type alias Score =
    Int


model : Model
model =
    { teams = [ "abc", "def" ]
    , matches = []
    , newTeamInput = ""
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        -- teams
        SetNewTeamInput value ->
            setNewTeamInput value model

        AddTeam teamName ->
            addTeam (Debug.log "name" teamName) model

        DeleteTeam teamName ->
            deleteTeam teamName model


setNewTeamInput : String -> Model -> Model
setNewTeamInput value model =
    { model | newTeamInput = value }


addTeam : String -> Model -> Model
addTeam teamName model =
    { model | teams = model.teams ++ [ teamName ] }
        |> setNewTeamInput ""


deleteTeam : String -> Model -> Model
deleteTeam teamName model =
    { model | teams = List.remove teamName model.teams }


suggestions : Model -> List ( Team, Team )
suggestions model =
    let
        all =
            model.teams
                |> combinations

        done =
            model.matches
                |> List.map
                    (\( ( t1, s1 ), ( t2, s2 ) ) ->
                        if t1 < t2 then
                            ( t1, t2 )
                        else
                            ( t2, t1 )
                    )
                |> Set.fromList

        waiting =
            done
                |> Set.diff (Set.fromList all)
    in
        waiting
            |> Set.toList
            |> List.take 3



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "page" ]
        [ viewHeader
        , viewHeaderBottom
        , viewTeams
        ]


viewHeader : Html Msg
viewHeader =
    H.div
        [ HA.class "header" ]
        [ H.div [ HA.class "header-left" ]
            [ H.h1 []
                [ H.text "ShipIt Leaderboard" ]
            , H.div
                [ HA.class "subheading" ]
                [ H.span
                    [ HA.class "date" ]
                    [ H.text "2017/09/21-22" ]
                , H.span
                    [ HA.class "format" ]
                    [ H.text "2v2" ]
                ]
            ]
        ]


viewHeaderBottom : Html Msg
viewHeaderBottom =
    H.div
        [ HA.class "header-bottom" ]
        []


viewTeams : Html Msg
viewTeams =
    H.div
        [ HA.class "teams" ]
        [ viewTeam False "JB4 + Matt" 2 3
        , viewTeam True "Jirka + Vašek" 4 1
        , viewTeam False "Dušan + Marín" 1 4
        ]


viewTeam : Bool -> String -> Int -> Int -> Html Msg
viewTeam isWinning team wins losses =
    H.div
        [ HA.classList
            [ ( "team", True )
            , ( "team--winning", isWinning )
            ]
        ]
        [ viewTeamColumn "team" "Team" team
        , viewTeamColumn "wins" "Wins" (toString wins)
        , viewTeamColumn "losses" "Losses" (toString losses)
        ]


viewTeamColumn : String -> String -> String -> Html Msg
viewTeamColumn classSuffix label value =
    H.div
        [ HA.class <| "column column--" ++ classSuffix ]
        [ H.div
            [ HA.class "label" ]
            [ H.text label ]
        , H.div
            [ HA.class <| "value value--" ++ classSuffix ]
            [ H.text value ]
        ]



-- HELPERS


combinations : List comparable -> List ( comparable, comparable )
combinations list =
    List.lift2 (,) list list
        |> List.filter (\( a, b ) -> a /= b)
        |> List.map
            (\( a, b ) ->
                if a < b then
                    ( a, b )
                else
                    ( b, a )
            )
        |> List.unique
