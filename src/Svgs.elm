module Svgs exposing (consecutiveVictorySvg, undoSvg, xSvg)

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
          (  "M" ++ String.fromFloat xOffset ++ "," ++ String.fromFloat yOffset
          ++ "v -11"
          ++ "a "
            ++ String.fromInt bodyRadius ++ "," ++ String.fromInt bodyRadius
            ++ " 0 0,1 "
            ++ String.fromInt (2 * bodyRadius) ++ ",0"
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
    

undoSvg : Html msg
undoSvg =
  svg
    [ viewBox "0 0 60 60"
    ]
    [ g
      [ ]
      [ Svg.Styled.path
        [ fill "currentColor"
        , d
          (  "M 8,33"
          ++ "A 22,22 0 1,0 30,13"
          ++ "h -4"
          ++ "l 10,-10"
          ++ "h -5"
          ++ "l -11,11"
          ++ "h 1.75"
          ++ "v -1"
          ++ "a 2,2 0 0,0 0,4"
          ++ "v -1"
          ++ "h -1.75"
          ++ "l 11,11"
          ++ "h 5"
          ++ "l -10,-10"
          ++ "h 4"
          ++ "A 18,18 0 1,1 12,33"
          ++ "a 2,2 0 1,0 -4,0"
          ++ "z"
          )
        ]
        []
      ]
    ]


xSvg : Html msg
xSvg =
  svg
    [ viewBox "0 0 60 60"
    ]
    [ g
      [ ]
      [ Svg.Styled.path
        [ fill "currentColor"
        , d
          (  "M 2,52"
          ++ "a 4,4 -45 0,0 6,6"
          ++ "L 30 36"
          ++ "L 52 58"
          ++ "a 4,4 45 0,0 6,-6"
          ++ "L 36 30"
          ++ "L 58 8"
          ++ "a 4,4 135 0,0 -6,-6"
          ++ "L 30 24"
          ++ "L 8 2"
          ++ "a 4,4 -135 0,0 -6,6"
          ++ "L 24 30"
          ++ "z"
          )
        ]
        []
      ]
    ]

