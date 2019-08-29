module HtmlHelper exposing (hr, range)

import Css exposing (..)
import Html.Styled exposing (Html, input)
import Html.Styled.Attributes exposing (css, type_, min, max, value, step)
import Html.Styled.Events exposing (onInput)


hr : Float -> Color -> List Style -> Html msg
hr heightPx color styles =
  Html.Styled.hr
    [ css
        [ display block
        , height (px heightPx)
        , border zero
        , borderTop3 (px heightPx) solid color
        , padding zero
        , margin zero
        , batch styles
        ]
    ]
    []


type alias RangeConfig msg =
  {
    min: Int
  , max: Int
  , step: Int
  , value: Int
  , onInput: (String -> msg)
  }

type alias RangeStyles =
  { height: Float
  , padding: Float
  , background: Color
  , trackBorderRadius: Float
  , thumbHeight: Float
  , thumbWidth: Float
  , thumbColor: Color
  , thumbBorderRadius: Float
  , additional: List Style
  }


range : RangeConfig msg -> RangeStyles -> Html msg
range config styles =
  input
    [ type_ "range"
    , min (String.fromInt config.min)
    , max (String.fromInt config.max)
    , step (String.fromInt config.step)
    , value (String.fromInt config.value)
    , onInput config.onInput
    , css (inputStyling styles)
    ]
    []


inputStyling : RangeStyles -> List Style
inputStyling styles =
  [ width (pct 100)
  , backgroundColor styles.background
  , borderRadius (px styles.trackBorderRadius)
  , padding (px styles.padding)
  , height (px styles.height)
  , boxSizing borderBox

  , property "-webkit-appearance" "none"
  , property "-webkit-tap-highlight-color" "transparent"
  , focus [ outline none ]
  , margin zero
  
  , pseudoElement "-webkit-slider-runnable-track" (inputTrackStyling styles)
  , pseudoElement "-moz-range-track" (inputTrackStyling styles)
  , pseudoElement "-ms-track" (inputTrackStyling styles)

  , pseudoElement "-webkit-slider-thumb" [ batch (inputThumbStyling styles) ]
  , pseudoElement "-moz-range-thumb" (inputThumbStyling styles)
  , pseudoElement "-ms-thumb" [ batch (inputThumbStyling styles), marginTop (px 1) ]

  , batch styles.additional
  ]


inputTrackStyling : RangeStyles -> List Style
inputTrackStyling styles =
  [ borderRadius (px styles.trackBorderRadius)
  , backgroundColor transparent

  , cursor pointer
  , boxShadow none
  , border zero
  ]


inputThumbStyling : RangeStyles -> List Style
inputThumbStyling styles =
  [ height (px styles.thumbHeight)
  , width (px styles.thumbWidth)
  , borderRadius (px styles.thumbBorderRadius)
  , backgroundColor styles.thumbColor

  , property "-webkit-appearance" "none"
  , cursor pointer
  , boxShadow none
  , border zero
  ]

