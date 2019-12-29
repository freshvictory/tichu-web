module Scorer exposing (..)


type alias Scorer =
    { north : ( Player, Bet )
    , south : ( Player, Bet )
    , east : ( Player, Bet )
    , west : ( Player, Bet )
    , firstOut : FirstOut
    , vertTurnScore : Int
    , vertScore : Int
    , horzScore : Int
    , history : List ( Int, Int )
    }


type Bet
    = Zero
    | Tichu
    | GrandTichu


type Player
    = North
    | South
    | East
    | West


type FirstOut
    = None
    | One Player
    | Team ( Team, Maybe Player )


type Team
    = Vertical
    | Horizontal


defaultScorer : Scorer
defaultScorer =
    { north = ( North, Zero )
    , south = ( South, Zero )
    , east = ( East, Zero )
    , west = ( West, Zero )
    , firstOut = None
    , vertTurnScore = 50
    , vertScore = 0
    , horzScore = 0
    , history = [ ( 0, 0 ) ]
    }


inTeam : Player -> Team -> Bool
inTeam player team =
    case team of
        Vertical ->
            player == North || player == South

        Horizontal ->
            player == East || player == West


getPlayerId : Player -> String
getPlayerId player =
    case player of
        North ->
            "north"

        South ->
            "south"

        East ->
            "east"

        West ->
            "west"


getTeamScore : Scorer -> ( Player, Bet ) -> ( Player, Bet ) -> Team -> Int -> Int
getTeamScore scorer player1 player2 team score =
    getPlayerBonus scorer player1
        + getPlayerBonus scorer player2
        + (case scorer.firstOut of
            Team ( t, _ ) ->
                if t == team then
                    200

                else
                    0

            _ ->
                score
          )


getPlayerBonus : Scorer -> ( Player, Bet ) -> Int
getPlayerBonus scorer ( player, bet ) =
    getScore bet
        * (case scorer.firstOut of
            One p ->
                if p == player then
                    1

                else
                    -1

            Team ( _, Just p ) ->
                if p == player then
                    1

                else
                    -1

            _ ->
                -1
          )


getScore : Bet -> Int
getScore bet =
    case bet of
        Zero ->
            0

        Tichu ->
            100

        GrandTichu ->
            200


changeTurnScore : Scorer -> Int -> Scorer
changeTurnScore scorer score =
    if score >= -25 && score <= 125 then
        { scorer | vertTurnScore = score }

    else
        scorer


changePlayerBet : Scorer -> Player -> Bet -> Scorer
changePlayerBet scorer player bet =
    case player of
        North ->
            { scorer | north = ( player, bet ) }

        South ->
            { scorer | south = ( player, bet ) }

        East ->
            { scorer | east = ( player, bet ) }

        West ->
            { scorer | west = ( player, bet ) }


changeFirstOut : Scorer -> Player -> Bool -> Scorer
changeFirstOut scorer player result =
    if result then
        let
            firstOut =
                case scorer.firstOut of
                    Team ( team, _ ) ->
                        if inTeam player team then
                            Team ( team, Just player )

                        else
                            One player

                    _ ->
                        One player
        in
        { scorer | firstOut = firstOut }

    else
        { scorer | firstOut = None }


consecutiveVictory : Scorer -> Team -> Bool -> Scorer
consecutiveVictory scorer team result =
    let
        firstOut =
            if not result then
                case scorer.firstOut of
                    Team ( t, Just p ) ->
                        One p

                    _ ->
                        None

            else
                case scorer.firstOut of
                    None ->
                        Team ( team, Nothing )

                    One player ->
                        if inTeam player team then
                            Team ( team, Just player )

                        else
                            Team ( team, Nothing )

                    Team ( t, p ) ->
                        if team == t then
                            Team ( t, p )

                        else
                            Team ( team, Nothing )
    in
    { scorer | firstOut = firstOut }


undo : Scorer -> Scorer
undo scorer =
    let
        ( ( vS, hS ), history ) =
            case scorer.history of
                [] ->
                    ( ( 0, 0 ), [ ( 0, 0 ) ] )

                [ x ] ->
                    ( x, [ ( 0, 0 ) ] )

                x :: xs ->
                    ( x, xs )
    in
    { scorer
        | vertScore = scorer.vertScore - vS
        , horzScore = scorer.horzScore - hS
        , history = history
    }


scoreAll : Scorer -> Scorer
scoreAll scorer =
    reset
        (let
            vertDiff =
                getTeamScore
                    scorer
                    scorer.north
                    scorer.south
                    Vertical
                    scorer.vertTurnScore

            horzDiff =
                getTeamScore
                    scorer
                    scorer.west
                    scorer.east
                    Horizontal
                    (100 - scorer.vertTurnScore)
         in
         { scorer
            | vertScore = scorer.vertScore + vertDiff
            , horzScore = scorer.horzScore + horzDiff
            , history = ( vertDiff, horzDiff ) :: scorer.history
         }
        )


reset : Scorer -> Scorer
reset scorer =
    { scorer
        | north = ( North, Zero )
        , south = ( South, Zero )
        , east = ( East, Zero )
        , west = ( West, Zero )
        , firstOut = None
        , vertTurnScore = 50
    }
