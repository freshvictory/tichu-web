import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

import Team as Team exposing (Team, PlayerNumber)
import Bet exposing (Bet)


-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = \model -> { title = "Tichu", body = [ model |> view |> toUnstyled ] }
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- MODEL

type alias Model =
  { team1: Team
  , team2: Team
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { team1 = Team.default "Team 1"
    , team2 = Team.default "Team 2"
    }
  , Cmd.none
  )


type TeamNumber
  = Team1
  | Team2


getTeam : Model -> TeamNumber -> Team
getTeam model teamNumber =
  case teamNumber of
    Team1 -> model.team1
    Team2 -> model.team2


getPlayerId : TeamNumber -> PlayerNumber -> String
getPlayerId teamNumber playerNumber =
  let
    teamName = case teamNumber of
      Team1 -> "team1"  
      Team2 -> "team2"
    playerName = case playerNumber of
      Team.Player1 -> "player1"
      Team.Player2 -> "player2"
  in
    teamName ++ "-" ++ playerName



-- UPDATE

type Msg
  = ChangeTeamName TeamNumber String
  | ChangePlayerName TeamNumber PlayerNumber String
  | ChangePlayerBet TeamNumber PlayerNumber Bet String
  | ChangePlayerBetResult TeamNumber PlayerNumber (Bool -> Bet) Bool
  | Score
  | ChangeTeamScore String
  | ConsecutiveVictory TeamNumber String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTeamName teamNumber name ->
      ( changeTeamName model teamNumber name, Cmd.none )
    ChangePlayerName teamNumber playerNumber name ->
      ( changePlayerName model teamNumber playerNumber name, Cmd.none )
    ChangePlayerBet teamNumber playerNumber bet _ ->
      ( changePlayerBet model teamNumber playerNumber bet, Cmd.none )
    ChangePlayerBetResult teamNumber playerNumber betType result ->
      ( changePlayerBetResult model teamNumber playerNumber betType result, Cmd.none )
    Score ->
      ( scoreAll model, Cmd.none )
    ChangeTeamScore val ->
      ( case String.toInt val of
          Nothing -> model
          Just s -> changeTeamScore model s            
      , Cmd.none
      )
    ConsecutiveVictory teamNumber _ ->
      ( consecutiveVictory model teamNumber, Cmd.none )


updateTeam : Model -> TeamNumber -> (Team -> Team) -> Model
updateTeam model teamNumber updater =
  let
    updated = updater (getTeam model teamNumber)
  in
    case teamNumber of
      Team1 ->
        { model | team1 = updated }
      Team2 ->
        { model | team2 = updated }  


changeTeamName : Model -> TeamNumber -> String -> Model
changeTeamName model teamNumber name =
  updateTeam model teamNumber (Team.updateName name)

changePlayerName : Model -> TeamNumber -> PlayerNumber -> String -> Model
changePlayerName model teamNumber playerNumber name =
  updateTeam model teamNumber (Team.updatePlayerName playerNumber name)


changePlayerBet : Model -> TeamNumber -> PlayerNumber -> Bet -> Model
changePlayerBet model teamNumber playerNumber bet =
  updateTeam model teamNumber (Team.updatePlayerBet playerNumber bet)


changePlayerBetResult : Model -> TeamNumber -> PlayerNumber -> (Bool -> Bet) -> Bool -> Model
changePlayerBetResult model teamNumber playerNumber betType result =
  updateTeam model teamNumber (Team.updatePlayerBet playerNumber (betType result))


scoreAll : Model -> Model
scoreAll model = reset (updateScore model)


changeTeamScore : Model -> Int -> Model
changeTeamScore model team1Score =
  { model
  | team1 = Team.updateTurnScore model.team1 team1Score
  , team2 = Team.updateTurnScore model.team2 (100 - team1Score)
  }


consecutiveVictory : Model -> TeamNumber -> Model
consecutiveVictory model teamNumber =
  case teamNumber of
    Team1 ->
      { model 
      | team1 = Team.updateConsecutiveVictory model.team1 True
      , team2 = Team.updateConsecutiveVictory model.team2 False
      }
    Team2 ->
      { model 
      | team1 = Team.updateConsecutiveVictory model.team1 False
      , team2 = Team.updateConsecutiveVictory model.team2 True
      }


updateScore : Model -> Model
updateScore model =
  let
    team1Score = Team.calculateScore model.team1
    team2Score = Team.calculateScore model.team2
  in
    { model
    | team1 = Team.updateScore model.team1 team1Score
    , team2 = Team.updateScore model.team2 team2Score
    }


reset : Model -> Model
reset model =
  { model | team1 = Team.reset model.team1, team2 = Team.reset model.team2 }


-- VIEW

view : Model -> Html Msg
view model =
  div [ css [ displayFlex, flexDirection column ] ]
    [ viewTeams model
    , viewTeamTurnScore model
    , viewConsecutiveVictory model
    , button [ onClick Score ] [ text "score" ]
    ]


viewTeams : Model -> Html Msg
viewTeams model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ viewTeam model Team1
    , viewTeam model Team2
    ]


viewTeam : Model -> TeamNumber -> Html Msg
viewTeam model teamNumber =
  div [ css [ displayFlex, flexDirection column ] ]
    [ viewTeamName model teamNumber
    , viewTeamScore model teamNumber
    , viewPlayer model teamNumber Team.Player1
    , viewPlayer model teamNumber Team.Player2
    ]
  

viewTeamName : Model -> TeamNumber -> Html Msg
viewTeamName model teamNumber =
  input
    [ placeholder (getTeam model teamNumber).name
    , value (getTeam model teamNumber).name
    , onInput (ChangeTeamName teamNumber)
    ]
    []
  

viewTeamScore : Model -> TeamNumber -> Html Msg
viewTeamScore model teamNumber =
  text (String.fromInt (getTeam model teamNumber).score)


viewPlayer : Model -> TeamNumber -> PlayerNumber -> Html Msg
viewPlayer model teamNumber playerNumber =
  div [ css [ displayFlex, flexDirection column ] ]
    [ viewPlayerName model teamNumber playerNumber
    , viewPlayerBet model teamNumber playerNumber
    ]
  

viewPlayerName : Model -> TeamNumber -> PlayerNumber -> Html Msg
viewPlayerName model teamNumber playerNumber =
  let
    team = getTeam model teamNumber
    name = (Team.getPlayer team playerNumber).name
  in
    input
      [ placeholder name
      , value name
      , onInput (ChangePlayerName teamNumber playerNumber)
      ]
      []


viewPlayerBet : Model -> TeamNumber -> PlayerNumber -> Html Msg
viewPlayerBet model teamNumber playerNumber =
  let
    team = getTeam model teamNumber
    playerId = getPlayerId teamNumber playerNumber
    bet = (Team.getPlayer team playerNumber).bet
    (betType, success) = Bet.destructure bet
  in  
    div [ css [ displayFlex, flexDirection column ] ]
      [ div [ css [ displayFlex, flexDirection row ] ]
          [ labeledRadio
              ("none" ++ "-" ++ playerId)
              "None"
              ("bet" ++ "-" ++ playerId)
              (bet == Bet.None)
              (ChangePlayerBet teamNumber playerNumber Bet.None)          
          , labeledRadio
              ("tichu" ++ "-" ++ playerId)
              "Tichu"
              ("bet" ++ "-" ++ playerId)
              (Bet.isBetType bet Bet.Tichu)
              (ChangePlayerBet teamNumber playerNumber (Bet.Tichu False))
          , labeledRadio
              ("grand" ++ "-" ++ playerId)
              "Grand Tichu"
              ("bet"  ++ "-" ++ playerId)
              (Bet.isBetType bet Bet.GrandTichu)
              (ChangePlayerBet teamNumber playerNumber (Bet.GrandTichu False))
          ]
      , if bet /= Bet.None then
          div []
            [ input
            [ type_ "checkbox"
            , id ("success" ++ "-" ++ playerId)
            , Html.Styled.Attributes.checked success
            , onCheck (ChangePlayerBetResult teamNumber playerNumber betType) ] []
            , label [ for ("success" ++ "-" ++ playerId) ] [ text "Successful" ]
            ]
        else
          text ""
      ]


viewTeamTurnScore : Model -> Html Msg
viewTeamTurnScore model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ text (String.fromInt model.team1.turnScore)
    , input
      [ type_ "range"
      , Html.Styled.Attributes.min "0"
      , Html.Styled.Attributes.max "100"
      , value (String.fromInt model.team1.turnScore)
      , step "5"
      , onInput ChangeTeamScore ] []
    , text (String.fromInt model.team2.turnScore)
    ]


viewConsecutiveVictory : Model -> Html Msg
viewConsecutiveVictory model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ labeledRadio
        "cv-team1"
        "Consecutive"
        "cv"
        model.team1.consecutiveVictory
        (ConsecutiveVictory Team1)
    , labeledRadio
        "cv-team2"
        "Consecutive"
        "cv"
        model.team2.consecutiveVictory
        (ConsecutiveVictory Team2)
    ]


labeledRadio : String -> String -> String -> Bool -> (String -> Msg) -> Html Msg
labeledRadio elemid elemlabel rgroup isChecked msg =
  div []
    [ input 
      [ type_ "radio"
      , id elemid
      , name rgroup
      , Html.Styled.Attributes.checked isChecked
      , onInput msg
      ] []
    , label [ for elemid ] [ text elemlabel ]
    ]
