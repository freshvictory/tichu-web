module Svgs exposing (consecutiveVictorySvg, undoSvg, xSvg, gearSvg, trashSvg)

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


gearSvg : Html msg
gearSvg =
  svg
    [ viewBox "0 0 60 60"
    ]
    [ g
      [ ]
      [ circle 
        [ cx "30"
        , cy "30"
        , r "12"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "8"
        ]
        []
      , Svg.Styled.path
        [ stroke "currentColor"
        , fill "none"
        , strokeWidth "6"
        , d -- 15.9,15.9
          (  "M 30,2"
          ++ "v 5.5"
          ++ "A 22.5,22.5 0 0,0 14.1,14.1"
          ++ "l -3.8891 -3.8891"
          ++ "l 3.8891 3.8891"
          ++ "A 22.5,22.5, 0 0,0 7.5,30"
          ++ "h -5.5"
          ++ "h 5.5"
          ++ "A 22.5,22.5 0 0,0 14.1,45.9"
          ++ "l -3.8891 3.8891"
          ++ "l 3.8891 -3.8891"
          ++ "A 22.5,22.5 0 0,0 30 52.5"
          ++ "v 5.5"
          ++ "v -5.5"
          ++ "A 22.5,22.5 0 0,0 45.9,45.9"
          ++ "l 3.8891 3.8891"
          ++ "l -3.8891 -3.8891"
          ++ "A 22.5,22.5, 0 0,0 52.5,30"
          ++ "h 5.5"
          ++ "h -5.5"
          ++ "A 22.5,22.5 0 0,0 45.9,14.1"
          ++ "l 3.8891 -3.8891"
          ++ "l -3.8891 3.8891"
          ++ "A 22.5,22.5 0 0,0 30,7.5"
          ++ "z"
          )
        ]
        []
      ]
    ]


trashSvg : Html msg
trashSvg =
  svg
    [ viewBox "0 0 60 60"
    ]
    [ g
      [ ]
      [ rect
        [ fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , x "10"
        , y "16"
        , rx "3"
        , width "40"
        , height "42"
        ]
        []
      , line
        [ x1 "20"
        , y1 "20"
        , x2 "20"
        , y2 "53"
        , stroke "currentColor"
        , strokeWidth "2"
        ]
        []
      , line
        [ x1 "30"
        , y1 "20"
        , x2 "30"
        , y2 "53"
        , stroke "currentColor"
        , strokeWidth "2"
        ]
        []
      , line
        [ x1 "40"
        , y1 "20"
        , x2 "40"
        , y2 "53"
        , stroke "currentColor"
        , strokeWidth "2"
        ]
        []
      , rect
        [ fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , x "6"
        , y "8"
        , rx "3"
        , width "48"
        , height "8"
        ]
        []
      , circle
        [ fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , cx "30"
        , cy "5"
        , r "3"
        ]
        []
      ]
    ]

