module Svgs exposing (consecutiveVictorySvg)

import Html.Styled exposing (Html)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)

consecutiveVictorySvg : Html msg
consecutiveVictorySvg =
  let
    leftX = 3.5
    leftY = 49
    rightX = leftX + 23
    rightY = leftY + 8
  in
  svg
    [ viewBox "0 0 60 60"
    ]
    [ g
        []
        [ consecutiveVictoryPerson leftX leftY
        , consecutiveVictoryPerson rightX rightY
        ]
    ]


consecutiveVictoryPerson : Float -> Float -> Svg msg
consecutiveVictoryPerson xOffset yOffset =
 let
    headRadius = 10
    bodyRadius = 15
  in
  g
    [ ]
    [ Svg.Styled.path
        [ stroke "#111"
        , strokeWidth "2"
        , fill "#FFF"
        , d
          (  "M" ++ (String.fromFloat xOffset) ++ "," ++ (String.fromFloat yOffset)
          ++ "v -11"
          ++ "a "
            ++ (String.fromInt bodyRadius) ++ "," ++ (String.fromInt bodyRadius)
            ++ " 0 0,1 "
            ++ (String.fromInt (2 * bodyRadius)) ++ ",0"
          ++ "v 11"
          ++ "z"
          )
        ]
        []
      , circle
        [ cx (String.fromFloat (xOffset + bodyRadius))
        , cy (String.fromFloat (yOffset - (2 * bodyRadius) - 6))
        , r (String.fromInt headRadius)
        , fill "#FFF"
        , stroke "#111"
        , strokeWidth "2"
        ]
        []
    ]
    

