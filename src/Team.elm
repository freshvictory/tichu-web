module Team exposing (..)

import Bet exposing (Bet)
import Player exposing (Player)

type alias Team =
  { name : String
  , score : Int
  , turnScore : Int
  , consecutiveVictory : Bool
  , player1 : Player
  , player2 : Player
  }


type PlayerNumber
  = Player1
  | Player2


default : String -> Team
default name =
  { name = name
  , score = 0
  , turnScore = 50
  , consecutiveVictory = False
  , player1 = Player.default "Player 1"
  , player2 = Player.default "Player 2"
  }


calculateScore : Team -> Int
calculateScore team =
    team.turnScore
  + Player.getScore team.player1
  + Player.getScore team.player2
  + if team.consecutiveVictory then 200 else 0


getPlayer : Team -> PlayerNumber -> Player
getPlayer team playerNumber =
  case playerNumber of
    Player1 -> team.player1
    Player2 -> team.player2


updateName : String -> Team -> Team
updateName name team =
  { team | name = name }


updateScore : Team -> Int -> Team
updateScore team score =
  { team | score = team.score + score }


updateTurnScore : Team -> Int -> Team
updateTurnScore team score =
  { team | turnScore = score }


updateConsecutiveVictory : Team -> Bool -> Team
updateConsecutiveVictory team isCV =
  { team | consecutiveVictory = isCV }


updatePlayer : PlayerNumber -> (Player -> Player) -> Team -> Team
updatePlayer playerNumber updater team =
  let
    player = updater (getPlayer team playerNumber)
  in
    case playerNumber of
      Player1 ->
        { team | player1 = player }
      Player2 ->
        { team | player2 = player }


updatePlayerName : PlayerNumber -> String -> Team -> Team
updatePlayerName playerNumber name =
  updatePlayer playerNumber (Player.updateName name)


updatePlayerBet : PlayerNumber -> Bet -> Team -> Team
updatePlayerBet playerNumber bet =
  updatePlayer playerNumber (Player.updateBet bet)


reset : Team -> Team
reset team =
  { team
  | turnScore = 50
  , player1 = Player.reset team.player1
  , player2 = Player.reset team.player2
  , consecutiveVictory = False
  }
