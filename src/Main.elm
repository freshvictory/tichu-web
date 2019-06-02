import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


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
  ( { team1 =
      { name = "Team 1"
      , score = 0
      , turnScore = 50
      , consecutiveVictory = False
      , player1 = { name = "Player 1", bet = None }
      , player2 = { name = "Player 2", bet = None }
      }
    , team2 =
      { name = "Team 2"
      , score = 0
      , turnScore = 50
      , consecutiveVictory = False
      , player1 = { name = "Player 1", bet = None }
      , player2 = { name = "Player 2", bet = None }
      }
    }
  , Cmd.none
  )


type alias Team =
  { name : String
  , score : Int
  , turnScore : Int
  , consecutiveVictory : Bool
  , player1 : Player
  , player2 : Player
  }


type alias Player =
  { name : String
  , bet : Bet
  }


type Bet
  = None
  | Tichu Bool
  | GrandTichu Bool


type PlayerNumber
  = Player1
  | Player2


type TeamNumber
  = Team1
  | Team2


type alias PlayerUpdater a = (Player -> a -> Player)


getTeam : Model -> TeamNumber -> Team
getTeam model teamNumber =
  case teamNumber of
    Team1 -> model.team1
    Team2 -> model.team2


getPlayer : Model -> TeamNumber -> PlayerNumber -> Player
getPlayer model teamNumber playerNumber =
  let
    team = getTeam model teamNumber
  in
    case playerNumber of
      Player1 -> team.player1
      Player2 -> team.player2


getPlayerId : TeamNumber -> PlayerNumber -> String
getPlayerId teamNumber playerNumber =
  let
    teamName = case teamNumber of
      Team1 -> "team1"  
      Team2 -> "team2"
    playerName = case playerNumber of
      Player1 -> "player1"
      Player2 -> "player2"
  in
    teamName ++ "-" ++ playerName


getTeamScore : Team -> Int
getTeamScore team =
    team.turnScore
  + getPlayerScore team.player1
  + getPlayerScore team.player2
  + if team.consecutiveVictory then 200 else 0


getPlayerScore : Player -> Int
getPlayerScore player =
  case player.bet of
      None ->
        0
      Tichu success ->
        100 * (if success then 1 else -1)
      GrandTichu success ->
        200 * (if success then 1 else -1)



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


changeTeamName : Model -> TeamNumber -> String -> Model
changeTeamName model teamNumber name =
  case teamNumber of
    Team1 ->
        { model | team1 = updateName model.team1 name }
    Team2 ->
        { model | team2 = updateName model.team2 name }


changePlayerName : Model -> TeamNumber -> PlayerNumber -> String -> Model
changePlayerName model teamNumber playerNumber name =
  case teamNumber of
    Team1 ->
      { model | team1 = updatePlayer model.team1 playerNumber name updatePlayerName }
    Team2 ->
      { model | team1 = updatePlayer model.team2 playerNumber name updatePlayerName }


changePlayerBet : Model -> TeamNumber -> PlayerNumber -> Bet -> Model
changePlayerBet model teamNumber playerNumber bet =
  case teamNumber of
    Team1 ->
      { model | team1 = updatePlayer model.team1 playerNumber bet updatePlayerBet }
    Team2 ->
      { model | team2 = updatePlayer model.team2 playerNumber bet updatePlayerBet }


changePlayerBetResult : Model -> TeamNumber -> PlayerNumber -> (Bool -> Bet) -> Bool -> Model
changePlayerBetResult model teamNumber playerNumber betType result =
  case teamNumber of
    Team1 ->
      { model | team1 = updatePlayer model.team1 playerNumber (betType result) updatePlayerBet }
    Team2 ->
      { model | team2 = updatePlayer model.team2 playerNumber (betType result) updatePlayerBet }


scoreAll : Model -> Model
scoreAll model = reset (updateScore model)


changeTeamScore : Model -> Int -> Model
changeTeamScore model team1Score =
  { model
  | team1 = updateTeamTurnScore model.team1 team1Score
  , team2 = updateTeamTurnScore model.team2 (100 - team1Score)
  }


consecutiveVictory : Model -> TeamNumber -> Model
consecutiveVictory model teamNumber =
  case teamNumber of
    Team1 ->
      { model 
      | team1 = updateTeamConsecutiveVictory model.team1 True
      , team2 = updateTeamConsecutiveVictory model.team2 False
      }
    Team2 ->
      { model 
      | team1 = updateTeamConsecutiveVictory model.team1 False
      , team2 = updateTeamConsecutiveVictory model.team2 True
      }


updateScore : Model -> Model
updateScore model =
  let
    team1Score = getTeamScore model.team1
    team2Score = getTeamScore model.team2
  in
    { model
    | team1 = updateTeamScore model.team1 team1Score
    , team2 = updateTeamScore model.team2 team2Score
    }


updateName : Team -> String -> Team
updateName team name =
  { team | name = name }


updateTeamScore : Team -> Int -> Team
updateTeamScore team score =
  { team | score = team.score + score }


updateTeamTurnScore : Team -> Int -> Team
updateTeamTurnScore team score =
  { team | turnScore = score }


updateTeamConsecutiveVictory : Team -> Bool -> Team
updateTeamConsecutiveVictory team isCV =
  { team | consecutiveVictory = isCV }


updatePlayer : Team -> PlayerNumber -> a -> PlayerUpdater a -> Team
updatePlayer team playerNumber val updater =
  case playerNumber of
      Player1 ->
          { team | player1 = updater team.player1 val }  
      Player2 ->
          { team | player2 = updater team.player2 val }


updatePlayerName : PlayerUpdater String
updatePlayerName player name =
  { player | name = name }


updatePlayerBet : PlayerUpdater Bet
updatePlayerBet player bet =
  { player | bet = bet }


reset : Model -> Model
reset model =
  { model | team1 = resetTeam model.team1, team2 = resetTeam model.team2 }


resetTeam : Team -> Team
resetTeam team =
  { team
  | turnScore = 50
  , player1 = resetPlayer team.player1
  , player2 = resetPlayer team.player2
  , consecutiveVictory = False
  }


resetPlayer : Player -> Player
resetPlayer player =
  { player | bet = None }


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
    , viewPlayer model teamNumber Player1
    , viewPlayer model teamNumber Player2
    ]
  

viewTeamName : Model -> TeamNumber -> Html Msg
viewTeamName model teamNumber =
  input [ placeholder (getTeam model teamNumber).name
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
  input [ placeholder (getPlayer model teamNumber playerNumber).name
        , value (getPlayer model teamNumber playerNumber).name
        , onInput (ChangePlayerName teamNumber playerNumber)
        ]
        []


viewPlayerBet : Model -> TeamNumber -> PlayerNumber -> Html Msg
viewPlayerBet model teamNumber playerNumber =
  let
    playerId = getPlayerId teamNumber playerNumber
  in  
    div [ css [ displayFlex, flexDirection column ] ]
      [ div [ css [ displayFlex, flexDirection row ] ]
          [ labeledRadio
              ("none" ++ "-" ++ playerId)
              "None"
              ("bet" ++ "-" ++ playerId)
              ((getPlayer model teamNumber playerNumber).bet == None)
              (ChangePlayerBet teamNumber playerNumber None)          
          , labeledRadio
              ("tichu" ++ "-" ++ playerId)
              "Tichu"
              ("bet" ++ "-" ++ playerId)
              (case (getPlayer model teamNumber playerNumber).bet of
                Tichu _ -> True
                _ -> False 
              )
              (ChangePlayerBet teamNumber playerNumber (Tichu False))
          , labeledRadio
              ("grand" ++ "-" ++ playerId)
              "Grand Tichu"
              ("bet"  ++ "-" ++ playerId)
              (case (getPlayer model teamNumber playerNumber).bet of
                GrandTichu _ -> True
                _ -> False 
              )
              (ChangePlayerBet teamNumber playerNumber (GrandTichu False))
          ]
      , let
          bet = (getPlayer model teamNumber playerNumber).bet
        in
          if bet /= None then
          let
            (betType, success) = case bet of
                None ->
                  (\s -> None, False)
                Tichu s ->
                    (Tichu, s)
                GrandTichu s ->
                  (GrandTichu, s)
                    
          in
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
