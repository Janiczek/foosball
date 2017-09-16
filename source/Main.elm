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


{-| Very high level overview:

  - The user can add teams that will play in the tournament.
  - The app wants everybody to play with everybody.
  - The app tracks who has played with who.
  - The user can add matches (pick first team, pick second team, pick the winner).
  - After everybody has played with everybody, the game shows the result

-}



-- INIT


type alias Model =
    { teams : Set Team
    , matches : List Match
    , matchStatus : MatchStatus
    , newTeamInput : String
    }


init : ( Model, Cmd Msg )
init =
    { teams = Set.empty
    , matches = []
    , matchStatus = NotPicking
    , newTeamInput = ""
    }
        |> withCmd focusNewTeam


type MatchStatus
    = NotPicking
    | PickingFirst
    | PickingSecond Team
    | PickingWinner Team Team


type alias Team =
    String


type alias Match =
    ( ( Team, Score ), ( Team, Score ) )


type alias Score =
    Int



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- CMDS


focusNewTeam : Cmd Msg
focusNewTeam =
    Dom.focus newTeamId
        |> Task.attempt (always NoOp)


newTeamId : String
newTeamId =
    "new-team"



-- UPDATE


type Msg
    = NoOp
    | SetNewTeamInput String
    | AddTeam Team
    | StartPicking
    | PickFirst Team
    | PickSecond Team
    | PickWinner Team


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            noOp model

        -- teams
        SetNewTeamInput value ->
            setNewTeamInput value model

        AddTeam teamName ->
            addTeam teamName model

        -- matches
        StartPicking ->
            startPicking model

        PickFirst team ->
            pickFirst team model

        PickSecond team ->
            pickSecond team model

        PickWinner team ->
            pickWinner team model


{-| because of Dom.focus
-}
noOp : Model -> ( Model, Cmd Msg )
noOp model =
    model
        |> withNoCmd


setNewTeamInput : String -> Model -> ( Model, Cmd Msg )
setNewTeamInput value model =
    { model | newTeamInput = value }
        |> withNoCmd


{-| add a new team, reset the input, focus the input again for faster writing
-}
addTeam : String -> Model -> ( Model, Cmd Msg )
addTeam teamName model =
    { model | teams = Set.insert teamName model.teams }
        |> setNewTeamInput ""
        |> addCmd focusNewTeam


{-| user clicked on "New Match", start the match input process

That means:

1.  (before anything) -- NotPicking
2.  User clicks on New Match -- StartPicking
3.  User clicks on a team 1 -- PickFirst
4.  User clicks on a team 2 -- PickSecond
5.  User clicks on a team that won -- PickWinner

-}
startPicking : Model -> ( Model, Cmd Msg )
startPicking model =
    { model | matchStatus = PickingFirst }
        |> withNoCmd


pickFirst : Team -> Model -> ( Model, Cmd Msg )
pickFirst team1 model =
    { model | matchStatus = PickingSecond team1 }
        |> withNoCmd


pickSecond : Team -> Model -> ( Model, Cmd Msg )
pickSecond team2 model =
    team1Name model.matchStatus
        |> Maybe.map (\team1 -> { model | matchStatus = PickingWinner team1 team2 })
        |> Maybe.withDefault model
        |> withNoCmd


pickWinner : Team -> Model -> ( Model, Cmd Msg )
pickWinner winningTeam model =
    matchResult model.matchStatus winningTeam
        |> Maybe.map
            (\match ->
                { model
                    | matchStatus = NotPicking
                    , matches = match :: model.matches
                }
            )
        |> Maybe.withDefault model
        |> withNoCmd



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
    H.div [ HA.class "header" ]
        [ H.div [ HA.class "header__column" ]
            [ H.h1 [] [ H.text "Foosball Leaderboard" ]
            , H.div [ HA.class "subheading" ]
                [ H.span [ HA.class "date" ] [ H.text "2017/09/21-22" ]
                , H.span [ HA.class "format" ] [ H.text "2v2" ]
                ]
            ]
        , H.div [ HA.class "header__column" ] [ viewNewMatchButton model ]
        ]


viewNewMatchButton : Model -> Html Msg
viewNewMatchButton model =
    let
        atLeastTwoTeams =
            Set.size model.teams >= 2

        matchesToDo =
            toDo model.teams model.matches

        isDisabled =
            -- not enough teams to play anything
            not atLeastTwoTeams
                -- already in the process of creating a match
                || (model.matchStatus /= NotPicking)
                -- no more matches to play
                || List.isEmpty matchesToDo
    in
        H.button
            [ HA.class "button--new-match"
            , HE.onClick StartPicking
            , HA.disabled isDisabled
            ]
            [ H.text "New Match" ]


{-| This is the dark shape connecting the header and the teams list.
Maybe it can be done with ::before or ::after?
Don't know, don't care anymore :) It works!
-}
viewHeaderBottom : Html Msg
viewHeaderBottom =
    H.div [ HA.class "header-bottom" ] []


viewTeams : Model -> Html Msg
viewTeams model =
    H.div
        [ HA.classList
            [ ( "teams", True )

            -- when picking teams in a match, they will pulsate on hover
            , ( "teams--picking", model.matchStatus /= NotPicking )
            ]
        ]
    <|
        (List.map (viewTeam model) (teamsInfo model))
            ++ [ viewNewTeam model.newTeamInput model.teams ]


isInMatch : Model -> Team -> Bool
isInMatch { matchStatus, teams, matches } name =
    case matchStatus of
        NotPicking ->
            False

        PickingFirst ->
            -- pick anybody ('A') that still has some matches to play
            canPickFirst name (toDo teams matches)

        PickingSecond team1 ->
            -- pick anybody ('B') that has to play against A
            name /= team1 && canPickSecond name team1 (toDo teams matches)

        PickingWinner team1 team2 ->
            -- pick either A or B
            name == team1 || name == team2


teamLabel : Model -> Team -> String
teamLabel model name =
    case model.matchStatus of
        NotPicking ->
            "Team"

        PickingFirst ->
            if isInMatch model name then
                "Pick Team 1"
            else
                "Already played"

        PickingSecond team1 ->
            if name == team1 then
                "Team 1"
            else if isInMatch model name then
                "Pick Team 2"
            else
                "Already played"

        PickingWinner team1 team2 ->
            if name == team1 || name == team2 then
                "Who won?"
            else
                "Not playing"


viewTeam : Model -> TeamInfo -> Html Msg
viewTeam model { isWinning, isNext, name, wins, losses, matchesLeft } =
    let
        isPicking =
            model.matchStatus /= NotPicking
    in
        H.div
            (teamAttributes name model
                ++ [ HA.classList
                        [ ( "team", True )
                        , ( "team--winning", not isPicking && isWinning )
                        , ( "team--next", not isPicking && isNext )
                        , ( "team--not-in-match", isPicking && not (isInMatch model name) )
                        ]
                   ]
            )
            [ viewTeamColumn "team" (teamLabel model name) name
            , viewTeamColumn "wins" "Wins" (toString wins)
            , viewTeamColumn "losses" "Losses" (toString losses)
            , viewTeamColumn "left" "Matches left" (toString matchesLeft)
            ]


teamAttributes : Team -> Model -> List (Attribute Msg)
teamAttributes team { matchStatus, teams, matches } =
    case matchStatus of
        NotPicking ->
            []

        PickingFirst ->
            if canPickFirst team (toDo teams matches) then
                [ HE.onClick (PickFirst team) ]
            else
                []

        PickingSecond team1 ->
            if team /= team1 && canPickSecond team team1 (toDo teams matches) then
                [ HE.onClick (PickSecond team) ]
            else
                []

        PickingWinner team1 team2 ->
            if team == team1 || team == team2 then
                [ HE.onClick (PickWinner team) ]
            else
                []


viewTeamColumn : String -> String -> String -> Html Msg
viewTeamColumn classSuffix label value =
    H.div [ HA.class <| "column column--" ++ classSuffix ]
        [ H.div [ HA.class "label" ] [ H.text label ]
        , H.div [ HA.class <| "value value--" ++ classSuffix ] [ H.text value ]
        ]


viewNewTeam : String -> Set Team -> Html Msg
viewNewTeam inputValue teams =
    H.div
        [ HA.class "team team--new" ]
        [ viewNewTeamInput inputValue
        , viewNewTeamButton inputValue teams
        ]


viewNewTeamInput : String -> Html Msg
viewNewTeamInput inputValue =
    H.div [ HA.class "column column--team" ]
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


viewNewTeamButton : String -> Set Team -> Html Msg
viewNewTeamButton inputValue teams =
    H.div [ HA.class "column column--button" ]
        [ H.div [ HA.class "label" ] []
        , H.div [ HA.class <| "value value--button" ]
            [ H.button
                [ HE.onClick (AddTeam inputValue)
                , HA.disabled (isNewTeamButtonDisabled inputValue teams)
                , HA.class "button--add-team"
                ]
                [ H.text "Add" ]
            ]
        ]


{-| Can't input empty name or name that is already present
-}
isNewTeamButtonDisabled : String -> Set Team -> Bool
isNewTeamButtonDisabled inputValue teams =
    String.isEmpty inputValue
        || Set.member inputValue teams



-- DERIVED DATA HELPERS


team1Name : MatchStatus -> Maybe String
team1Name matchStatus =
    case matchStatus of
        NotPicking ->
            Nothing

        PickingFirst ->
            Nothing

        PickingSecond team1 ->
            Just team1

        PickingWinner team1 _ ->
            Just team1


matchResult : MatchStatus -> Team -> Maybe Match
matchResult matchStatus winnerTeam =
    case matchStatus of
        NotPicking ->
            Nothing

        PickingFirst ->
            Nothing

        PickingSecond _ ->
            Nothing

        PickingWinner team1 team2 ->
            let
                ( score1, score2 ) =
                    if winnerTeam == team1 then
                        ( 1, 0 )
                    else
                        ( 0, 1 )
            in
                Just ( ( team1, score1 ), ( team2, score2 ) )


{-| Team exists in matches left to play
-}
canPickFirst : Team -> List ( Team, Team ) -> Bool
canPickFirst team leftToDo =
    leftToDo
        |> List.any (\( t1, t2 ) -> t1 == team || t2 == team)


{-| Team exists in matches left to play with the team1
-}
canPickSecond : Team -> Team -> List ( Team, Team ) -> Bool
canPickSecond team team1 leftToDo =
    leftToDo
        |> List.any (\( t1, t2 ) -> (t1 == team1 && t2 == team) || (t2 == team1 && t1 == team))


{-| Sort teams inside the tuple (for List.unique filtering later). If only we had unordered tuples ;)

Maybe refactor (Team, Team) into Set Team?

-}
normalizeTeams : ( Team, Team ) -> ( Team, Team )
normalizeTeams ( team1, team2 ) =
    if team1 < team2 then
        ( team1, team2 )
    else
        ( team2, team1 )


{-|

    combinations [1,2,3]   == [(1,2),(1,3),(2,3)]
    combinations [1,2,3,4] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-}
combinations : List Team -> List ( Team, Team )
combinations list =
    List.lift2 (,) list list
        |> List.filter (\( a, b ) -> a /= b)
        |> List.map normalizeTeams
        |> List.unique


{-| Matches still left to play.
-}
toDo : Set Team -> List Match -> List ( Team, Team )
toDo teams matches =
    let
        allMatches : List ( Team, Team )
        allMatches =
            teams
                |> Set.toList
                |> combinations

        done : List ( Team, Team )
        done =
            matches
                |> List.map (\( ( t1, s1 ), ( t2, s2 ) ) -> ( t1, t2 ))
                |> List.map normalizeTeams
    in
        Set.diff
            (Set.fromList allMatches)
            (Set.fromList done)
            |> Set.toList


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


type alias TeamInfo =
    { name : String
    , isWinning : Bool
    , isNext : Bool
    , wins : Int
    , losses : Int
    , matchesLeft : Int
    }


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
                        ( ( team1, teamToDoCount countsLeft team1 )
                        , ( team2, teamToDoCount countsLeft team2 )
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
                    { name = team
                    , isWinning = isWinning leftToDo winningTeam team
                    , isNext = isNext (teamToDoCount countsLeft team) nextTeams team
                    , wins = wins team matches
                    , losses = losses team matches
                    , matchesLeft = teamToDoCount countsLeft team
                    }
                )


teamToDoCount : Dict Team Int -> Team -> Int
teamToDoCount countsLeft team =
    countsLeft
        |> Dict.get team
        |> Maybe.withDefault 0


isWinning : List ( Team, Team ) -> Maybe Team -> Team -> Bool
isWinning leftToDo winningTeam team =
    (List.length leftToDo == 0)
        && (winningTeam == Just team)


isNext : Int -> Maybe ( Team, Team ) -> Team -> Bool
isNext matchesLeft nextTeams team =
    (matchesLeft > 0)
        && (nextTeams
                |> Maybe.map (\( a, b ) -> a == team || b == team)
                |> Maybe.withDefault False
           )
