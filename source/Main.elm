module Main exposing (..)

import Set exposing (Set)
import Task
import Dict exposing (Dict)
import List.Extra as List
import Cmd.Extra exposing (..)
import Dom
import Html as H exposing (Html, Attribute)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra as HE


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { teams : Set Team
    , matches : List Match

    -- inputs
    , newTeamInput : String
    , matchStatus : MatchStatus
    }


type MatchStatus
    = NotPicking
    | PickingFirst
    | PickingSecond Team
    | AwaitingResult Team Team


type Msg
    = NoOp
    | SetNewTeamInput String
    | AddTeam Team
    | StartPicking
    | PickFirst Team
    | PickSecond Team
    | SetWinner Team


type alias Team =
    String


type alias TeamInfo =
    { name : String
    , isWinning : Bool
    , isNext : Bool
    , wins : Int
    , losses : Int
    , matchesLeft : Int
    }


type alias Match =
    ( ( Team, Score ), ( Team, Score ) )


type alias Score =
    Int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    { teams = Set.empty
    , matches = []
    , newTeamInput = ""
    , matchStatus = NotPicking
    }
        |> withCmd focusNewTeam


newTeamId : String
newTeamId =
    "new-team"


focusNewTeam : Cmd Msg
focusNewTeam =
    Dom.focus newTeamId
        |> Task.attempt (always NoOp)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                |> withNoCmd

        -- teams
        SetNewTeamInput value ->
            setNewTeamInput value model
                |> withNoCmd

        AddTeam teamName ->
            addTeam teamName model
                |> withCmd focusNewTeam

        StartPicking ->
            startPicking model
                |> withNoCmd

        PickFirst team ->
            pickFirst team model
                |> withNoCmd

        PickSecond team ->
            pickSecond team model
                |> withNoCmd

        SetWinner team ->
            setWinner team model
                |> withNoCmd


setNewTeamInput : String -> Model -> Model
setNewTeamInput value model =
    { model | newTeamInput = value }


addTeam : String -> Model -> Model
addTeam teamName model =
    { model | teams = Set.insert teamName model.teams }
        |> setNewTeamInput ""


startPicking : Model -> Model
startPicking model =
    { model | matchStatus = PickingFirst }


pickFirst : Team -> Model -> Model
pickFirst team1 model =
    { model | matchStatus = PickingSecond team1 }


pickSecond : Team -> Model -> Model
pickSecond team2 model =
    case model.matchStatus of
        PickingSecond team1 ->
            { model | matchStatus = AwaitingResult team1 team2 }

        _ ->
            model


setWinner : Team -> Model -> Model
setWinner winningTeam model =
    case model.matchStatus of
        AwaitingResult team1 team2 ->
            let
                ( score1, score2 ) =
                    if team1 == winningTeam then
                        ( 1, 0 )
                    else
                        ( 0, 1 )
            in
                { model
                    | matchStatus = NotPicking
                    , matches = ( ( team1, score1 ), ( team2, score2 ) ) :: model.matches
                }

        _ ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "page" ]
        [ viewHeader model
        , viewHeaderBottom
        , viewTeams model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        atLeastTwoTeams =
            Set.size model.teams >= 2
    in
        H.div [ HA.class "header" ]
            [ H.div [ HA.class "header__column" ]
                [ H.h1 [] [ H.text "Foosball Leaderboard" ]
                , H.div [ HA.class "subheading" ]
                    [ H.span [ HA.class "date" ] [ H.text "2017/09/21-22" ]
                    , H.span [ HA.class "format" ] [ H.text "2v2" ]
                    ]
                ]
            , H.div [ HA.class "header__column" ]
                [ H.button
                    [ HA.class "button--new-match"
                    , HE.onClick StartPicking
                    , HA.disabled
                        ((not atLeastTwoTeams)
                            || (model.matchStatus /= NotPicking)
                            || (List.length (toDo model.teams model.matches) == 0)
                        )
                    ]
                    [ H.text "New Match" ]
                ]
            ]


viewHeaderBottom : Html Msg
viewHeaderBottom =
    H.div [ HA.class "header-bottom" ] []


viewTeams : Model -> Html Msg
viewTeams model =
    let
        teams =
            teamsInfo model
    in
        H.div
            [ HA.classList
                [ ( "teams", True )
                , ( "teams--picking", model.matchStatus /= NotPicking )
                ]
            ]
        <|
            (List.map (viewTeam model.matchStatus model.teams model.matches) teams)
                ++ [ viewNewTeam model.newTeamInput model.teams ]


viewTeam : MatchStatus -> Set Team -> List Match -> TeamInfo -> Html Msg
viewTeam matchStatus teams matches { isWinning, isNext, name, wins, losses, matchesLeft } =
    let
        leftToDo =
            toDo teams matches

        ( isPicking, disableSomeTeams, isInMatch ) =
            case matchStatus of
                NotPicking ->
                    ( False, False, False )

                PickingFirst ->
                    ( True, True, canPickFirst name leftToDo )

                PickingSecond team1 ->
                    ( True, True, name /= team1 && canPickSecond name team1 leftToDo )

                AwaitingResult team1 team2 ->
                    ( True, True, name == team1 || name == team2 )
    in
        H.div
            (maybeClickableTeam name matchStatus teams matches
                ++ [ HA.classList
                        [ ( "team", True )
                        , ( "team--winning", not isPicking && isWinning )
                        , ( "team--next", not isPicking && isNext )
                        , ( "team--not-in-match", disableSomeTeams && not isInMatch )
                        ]
                   ]
            )
            [ viewTeamColumn "team"
                (case matchStatus of
                    NotPicking ->
                        "Team"

                    PickingFirst ->
                        if isInMatch then
                            "Pick Team 1"
                        else
                            "Already played"

                    PickingSecond team1 ->
                        if name == team1 then
                            "Team 1"
                        else if isInMatch then
                            "Pick Team 2"
                        else
                            "Already played"

                    AwaitingResult team1 team2 ->
                        if name == team1 || name == team2 then
                            "Who won?"
                        else
                            "Not playing"
                )
                name
            , viewTeamColumn "wins" "Wins" (toString wins)
            , viewTeamColumn "losses" "Losses" (toString losses)
            , viewTeamColumn "left" "Matches left" (toString matchesLeft)
            ]


maybeClickableTeam : Team -> MatchStatus -> Set Team -> List Match -> List (Attribute Msg)
maybeClickableTeam team matchStatus teams matches =
    let
        leftToDo =
            toDo teams matches
    in
        case matchStatus of
            NotPicking ->
                []

            PickingFirst ->
                if canPickFirst team leftToDo then
                    [ HE.onClick (PickFirst team) ]
                else
                    []

            PickingSecond team1 ->
                if team /= team1 && canPickSecond team team1 leftToDo then
                    [ HE.onClick (PickSecond team) ]
                else
                    []

            AwaitingResult team1 team2 ->
                if team == team1 || team == team2 then
                    [ HE.onClick (SetWinner team) ]
                else
                    []


canPickFirst : Team -> List ( Team, Team ) -> Bool
canPickFirst team leftToDo =
    leftToDo
        |> List.any (\( t1, t2 ) -> t1 == team || t2 == team)


canPickSecond : Team -> Team -> List ( Team, Team ) -> Bool
canPickSecond team team1 leftToDo =
    leftToDo
        |> List.any (\( t1, t2 ) -> (t1 == team1 && t2 == team) || (t2 == team1 && t1 == team))


viewTeamColumn : String -> String -> String -> Html Msg
viewTeamColumn classSuffix label value =
    H.div [ HA.class <| "column column--" ++ classSuffix ]
        [ H.div [ HA.class "label" ] [ H.text label ]
        , H.div [ HA.class <| "value value--" ++ classSuffix ] [ H.text value ]
        ]


viewNewTeam : String -> Set Team -> Html Msg
viewNewTeam inputValue teams =
    let
        isDisabled =
            String.isEmpty inputValue
                || Set.member inputValue teams
    in
        H.div
            [ HA.class "team team--new" ]
            [ H.div [ HA.class "column column--team" ]
                [ H.div [ HA.class "label" ] [ H.text "New Team" ]
                , H.div [ HA.class <| "value value--team" ]
                    [ H.input
                        [ HA.id newTeamId
                        , HE.onInput SetNewTeamInput
                        , HE.onEnter (AddTeam inputValue)
                        , HA.type_ "text"
                        , HA.class "input--team"
                        , HA.value inputValue
                        ]
                        []
                    ]
                ]
            , H.div [ HA.class "column column--button" ]
                [ H.div [ HA.class "label" ] []
                , H.div [ HA.class <| "value value--button" ]
                    [ H.button
                        [ HE.onClick (AddTeam inputValue)
                        , HA.disabled isDisabled
                        , HA.class "button--add-team"
                        ]
                        [ H.text "Add" ]
                    ]
                ]
            ]



-- HELPERS


normalizeMatch : ( comparable, comparable ) -> ( comparable, comparable )
normalizeMatch ( a, b ) =
    if a < b then
        ( a, b )
    else
        ( b, a )


combinations : Set comparable -> List ( comparable, comparable )
combinations set =
    let
        list =
            Set.toList set
    in
        List.lift2 (,) list list
            |> List.filter (\( a, b ) -> a /= b)
            |> List.map normalizeMatch
            |> List.unique


toDo : Set Team -> List Match -> List ( Team, Team )
toDo teams matches =
    let
        allMatches : List ( Team, Team )
        allMatches =
            combinations teams

        done : List ( Team, Team )
        done =
            matches
                |> List.map (\( ( t1, s1 ), ( t2, s2 ) ) -> ( t1, t2 ))
                |> List.map normalizeMatch
    in
        Set.diff
            (Set.fromList allMatches)
            (Set.fromList done)
            |> Set.toList


teamsInfo : Model -> List TeamInfo
teamsInfo { teams, matches } =
    let
        leftToDo : List ( Team, Team )
        leftToDo =
            toDo teams matches

        countsLeft : Dict Team Int
        countsLeft =
            teams
                |> Set.toList
                |> List.map (countMatches leftToDo)
                |> Dict.fromList

        teamWins : List ( Team, Int )
        teamWins =
            teams
                |> Set.toList
                |> List.map (\team -> ( team, wins team matches ))
                |> List.sortBy (\( team, wins ) -> wins)
                |> List.reverse

        biggestWin : Int
        biggestWin =
            teamWins
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0

        teamsWithBiggestWin : Int
        teamsWithBiggestWin =
            teamWins
                |> List.filter (\( team, wins ) -> wins == biggestWin)
                |> List.length

        winningTeam : Maybe Team
        winningTeam =
            if teamsWithBiggestWin > 1 || Set.size teams < 2 then
                Nothing
            else
                teamWins
                    |> List.head
                    |> Maybe.map Tuple.first

        allNextTeams : List ( ( Team, Int ), ( Team, Int ) )
        allNextTeams =
            leftToDo
                |> List.map
                    (\( team1, team2 ) ->
                        ( ( team1, countsLeft |> Dict.get team1 |> Maybe.withDefault 0 )
                        , ( team2, countsLeft |> Dict.get team2 |> Maybe.withDefault 0 )
                        )
                    )
                |> List.sortBy (\( ( t1, s1 ), ( t2, s2 ) ) -> (s1 + s2) // 2)

        nextTeams : Maybe ( Team, Team )
        nextTeams =
            case allNextTeams of
                ( ( t1, s1 ), ( t2, s2 ) ) :: _ ->
                    Just ( t1, t2 )

                _ ->
                    Nothing
    in
        teams
            |> Set.toList
            |> List.map
                (\team ->
                    let
                        matchesLeft =
                            countsLeft
                                |> Dict.get team
                                |> Maybe.withDefault 0
                    in
                        { name = team
                        , isWinning = List.length leftToDo == 0 && winningTeam == Just team
                        , isNext =
                            (matchesLeft > 0)
                                && (nextTeams
                                        |> Maybe.map (\( a, b ) -> a == team || b == team)
                                        |> Maybe.withDefault False
                                   )
                        , wins = wins team matches
                        , losses = losses team matches
                        , matchesLeft = matchesLeft
                        }
                )


wins : Team -> List Match -> Int
wins team matches =
    matches
        |> List.filter (\( ( t1, s1 ), ( t2, s2 ) ) -> t1 == team && s1 > s2 || t2 == team && s2 > s1)
        |> List.length


losses : Team -> List Match -> Int
losses team matches =
    matches
        |> List.filter (\( ( t1, s1 ), ( t2, s2 ) ) -> t1 == team && s1 < s2 || t2 == team && s2 < s1)
        |> List.length


countMatches : List ( Team, Team ) -> Team -> ( Team, Int )
countMatches matches team =
    let
        count =
            matches
                |> List.filter (\( a, b ) -> a == team || b == team)
                |> List.length
    in
        ( team, count )
