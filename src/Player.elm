module Player exposing (..)

import Bet exposing (Bet)

type alias Player =
  { name : String
  , bet : Bet
  }


default : String -> Player
default name =
  { name = name
  , bet = Bet.default
  }


updateName : String -> Player -> Player
updateName name player =
  { player | name = name }


updateBet : Bet -> Player -> Player
updateBet bet player =
  { player | bet = bet }


reset : Player -> Player
reset player =
  { player | bet = Bet.default }

getScore : Player -> Int
getScore player = Bet.getScore player.bet
