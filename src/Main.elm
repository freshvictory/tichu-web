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
  { north: (Player, Bet)
  , south: (Player, Bet)
  , east: (Player, Bet)
  , west: (Player, Bet)
  , firstOut: FirstOut
  , vertTurnScore: Int
  , vertScore: Int
  , horzScore: Int
  , crashed: Bool
  }


type Bet
  = Zero
  | Tichu
  | GrandTichu


default : Bet
default = Zero


getScore : Bet -> Int
getScore bet =
  case bet of
      Zero ->
        0
      Tichu ->
        100
      GrandTichu ->
        200


type Player
  = North
  | South
  | East
  | West


type FirstOut
  = None
  | One Player
  | Team (Team, (Maybe Player))


type Team
  = Vertical
  | Horizontal


inTeam : Player -> Team -> Bool
inTeam player team =
  case team of
    Vertical -> player == North || player == South
    Horizontal -> player == East || player == West


getPlayerId : Player -> String
getPlayerId player =
  case player of
    North -> "north"
    South -> "south"
    East -> "east"
    West -> "west"


init : () -> ( Model, Cmd Msg )
init _ =
  ( { north = (North, Zero)
    , south = (South, Zero)
    , east = (East, Zero)
    , west = (West, Zero)
    , firstOut = None
    , vertTurnScore = 50
    , vertScore = 0
    , horzScore = 0
    , crashed = False
    }
  , Cmd.none
  )



-- UPDATE

type Msg
  = ChangePlayerBet Player Bet String
  | ChangeFirstOut Player Bool
  | ChangeTeamScore String
  | ConsecutiveVictory Team Bool
  | CrashApp
  | Score


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePlayerBet playerType bet _ ->
      ( changePlayerBet model playerType bet, Cmd.none )
    ChangeFirstOut playerType result ->
      ( changeFirstOut model playerType result, Cmd.none )
    Score ->
      ( scoreAll model, Cmd.none )
    ChangeTeamScore val ->
      ( case String.toInt val of
          Nothing -> model
          Just s -> { model | vertTurnScore = s }
      , Cmd.none
      )
    ConsecutiveVictory team result ->
      ( consecutiveVictory model team result, Cmd.none )
    CrashApp ->
      ( { model | crashed = True }, Cmd.none )


changePlayerBet : Model -> Player -> Bet -> Model
changePlayerBet model player bet =
  case player of
    North -> { model | north = (player, bet) }
    South -> { model | south = (player, bet) }
    East -> { model | east = (player, bet) }
    West -> { model | west = (player, bet) }
  


changeFirstOut : Model -> Player -> Bool -> Model
changeFirstOut model player result =
  if result then
    let        
      firstOut =
        case model.firstOut of
          Team (team, _) -> if inTeam player team then Team (team, Just player) else One player
          _ -> One player
    in    
      { model | firstOut = firstOut }
  else
    { model | firstOut = None }


consecutiveVictory : Model -> Team -> Bool -> Model
consecutiveVictory model team result =
  let
    firstOut =
      if not result then
        case model.firstOut of
          Team (t, Just p) -> One p
          _ -> None
      else
        case model.firstOut of
          None -> Team (team, Nothing)
          One player -> if inTeam player team then Team (team, Just player) else Team (team, Nothing)
          Team (t, p) -> if team == t then Team (t, p) else Team (team, Nothing)
  in
    { model | firstOut = firstOut }
    


scoreAll : Model -> Model
scoreAll model =
  reset (
    let
      vertScore
        = model.vertScore
        + getPlayerBonus model model.north
        + getPlayerBonus model model.south
        + (case model.firstOut of
            Team (Vertical, _) -> 200
            Team (Horizontal, _) -> 0      
            _ -> model.vertTurnScore
          )
      horzScore
        = model.horzScore
        + getPlayerBonus model model.west
        + getPlayerBonus model model.east
        + (case model.firstOut of
            Team (Horizontal, _) -> 200
            Team (Vertical, _) -> 0
            _ -> (100 - model.vertTurnScore)
          )
    in
      { model | vertScore = vertScore, horzScore = horzScore }
  )


getPlayerBonus : Model -> (Player, Bet) -> Int
getPlayerBonus model (player, bet) =
  (getScore bet) * (
    case model.firstOut of
      One p -> if p == player then 1 else -1
      Team (_, Just p) -> if p == player then 1 else -1
      _ -> -1
  )


reset : Model -> Model
reset model =
  { model
  | north = (North, Zero)
  , south = (South, Zero)
  , east = (East, Zero)
  , west = (West, Zero)
  , firstOut = None
  , vertTurnScore = 50
  }


-- VIEW

view : Model -> Html Msg
view model =
  if model.crashed then
    text "The app crashed :("
  else 
    div [ css [ displayFlex, flexDirection column ] ]
      [ button [ onClick CrashApp ] [ text "Things are not looking good" ]
      , viewTeams model
      , viewTeamTurnScore model
      , viewConsecutiveVictory model
      , button [ onClick Score ] [ text "score" ]
      ]


viewTeams : Model -> Html Msg
viewTeams model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ viewTeam model model.vertScore model.north model.south
    , viewTeam model model.horzScore model.east model.west
    ]


viewTeam : Model -> Int -> (Player, Bet) -> (Player, Bet) -> Html Msg
viewTeam model score player1 player2 =
  div [ css [ displayFlex, flexDirection column ] ]
    [ text (String.fromInt score)
    , viewPlayer model player1
    , viewPlayer model player2
    ]


viewPlayer : Model -> (Player, Bet) -> Html Msg
viewPlayer model playerBet =
  div [ css [ displayFlex, flexDirection column ] ]
    [ viewPlayerBet model playerBet
    ]


viewPlayerBet : Model -> (Player, Bet) -> Html Msg
viewPlayerBet model (player, bet) =
  let
    playerId = getPlayerId player
  in  
    div [ css [ displayFlex, flexDirection column ] ]
      [ div [ css [ displayFlex, flexDirection row ] ]
          [ labeledRadio
              ("none" ++ "-" ++ playerId)
              "None"
              ("bet" ++ "-" ++ playerId)
              (bet == Zero)
              (ChangePlayerBet player Zero)          
          , labeledRadio
              ("tichu" ++ "-" ++ playerId)
              "Tichu"
              ("bet" ++ "-" ++ playerId)
              (bet == Tichu)
              (ChangePlayerBet player Tichu)
          , labeledRadio
              ("grand" ++ "-" ++ playerId)
              "Grand Tichu"
              ("bet"  ++ "-" ++ playerId)
              (bet == GrandTichu)
              (ChangePlayerBet player GrandTichu)
          ]
      , if bet /= Zero then
          div []
            [ input
            [ type_ "checkbox"
            , id ("success" ++ "-" ++ playerId)
            , Html.Styled.Attributes.checked (
              case model.firstOut of
                One p -> p == player
                Team (_, Just p) -> p == player
                _ -> False 
            )
            , onCheck (ChangeFirstOut player) ] []
            , label [ for ("success" ++ "-" ++ playerId) ] [ text "Successful" ]
            ]
        else
          text ""
      ]


viewTeamTurnScore : Model -> Html Msg
viewTeamTurnScore model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ text (String.fromInt model.vertTurnScore)
    , input
      [ type_ "range"
      , Html.Styled.Attributes.min "-25"
      , Html.Styled.Attributes.max "125"
      , value (String.fromInt model.vertTurnScore)
      , step "5"
      , onInput ChangeTeamScore ] []
    , text (String.fromInt (100 - model.vertTurnScore))
    ]


viewConsecutiveVictory : Model -> Html Msg
viewConsecutiveVictory model =
  div [ css [ displayFlex, flexDirection row ] ]
    [ input
      [ type_ "checkbox"
      , id "vert-cv"
      , Html.Styled.Attributes.checked
        (case model.firstOut of
          Team (Vertical, _) -> True      
          _ -> False
        )
      , onCheck (ConsecutiveVictory Vertical)
      ][]
    , label [ for "vert-cv" ] [ text "Consecutive" ]
    , input
      [ type_ "checkbox"
      , id "horz-cv"
      , Html.Styled.Attributes.checked
        (case model.firstOut of
          Team (Horizontal, _) -> True      
          _ -> False
        )
      , onCheck (ConsecutiveVictory Horizontal)
      ][]
    , label [ for "horz-cv" ] [ text "Consecutive" ]
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
