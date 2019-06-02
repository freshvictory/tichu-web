module Bet exposing (..)


type Bet
  = None
  | Tichu Bool
  | GrandTichu Bool


default : Bet
default = None


getScore : Bet -> Int
getScore bet =
  case bet of
      None ->
        0
      Tichu success ->
        100 * (if success then 1 else -1)
      GrandTichu success ->
        200 * (if success then 1 else -1)


isBetType : Bet -> (Bool -> Bet) -> Bool
isBetType bet betType =
  let
    (bType, success) = destructure bet
  in
    bType False == betType False


destructure : Bet -> (Bool -> Bet, Bool)
destructure bet =
  case bet of
    None ->
      (\s -> None, False)
    Tichu s ->
      (Tichu, s)
    GrandTichu s ->
      (GrandTichu, s)
