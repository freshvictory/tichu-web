port module Main exposing (..)

import Browser
import Browser.Navigation
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Scorer exposing (..)


-- MAIN

main : Program Json.Decode.Value Model Msg
main =
  Browser.document
    { init = init
    , view = \model -> { title = "Tichu", body = [ model |> view |> toUnstyled ] }
    , update = updateWithStorage
    , subscriptions = subscriptions
    }


port setStorage : State -> Cmd msg

port updateAvailable : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    updateAvailable (\_ -> UpdateAvailable)


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (getState newModel), cmds ]
        )


-- MODEL

type alias Model =
  { scorer: Scorer
  , vertName: String
  , horzName: String
  , lighting: Lighting
  , showSettings: Bool
  , updateAvailable: Bool
  , crashed: Bool
  , confirm: Confirm
  }


defaultModel : Lighting -> String -> String -> Model
defaultModel lighting vertName horzName =
  { scorer = defaultScorer
  , vertName = vertName
  , horzName = horzName
  , lighting = lighting
  , showSettings = False
  , updateAvailable = False
  , crashed = False
  , confirm = Hidden
  }


modelFromState : State -> Model
modelFromState state =
  let
    model = defaultModel
      (if state.lighting == "dark" then Dark else Light)
      state.vertName
      state.horzName
    scorer = model.scorer
    newScorer = 
      { scorer
      | vertScore = state.vertScore
      , horzScore = state.horzScore
      , history = state.history
      }
  in
    { model | scorer = newScorer }


type alias State =
  { lighting: String
  , vertName: String
  , horzName: String
  , vertScore: Int
  , horzScore: Int
  , history: List (Int, Int)
  }


decodeState : Decoder State
decodeState =
  Json.Decode.map6 State
    (field "lighting" string)
    (field "vertName" string)
    (field "horzName" string)
    (field "vertScore" Json.Decode.int)
    (field "horzScore" Json.Decode.int)
    (field "history" (Json.Decode.list (decodeAsTuple2 "0" Json.Decode.int "1" Json.Decode.int)))


getState : Model -> State
getState model =
  { lighting = if model.lighting == Light then "light" else "dark"
  , vertName = model.vertName
  , horzName = model.horzName
  , vertScore = model.scorer.vertScore
  , horzScore = model.scorer.horzScore
  , history = model.scorer.history
  }


type Lighting
  = Light
  | Dark


type Confirm
  = Hidden
  | Active String Msg


init : Json.Decode.Value -> ( Model, Cmd Msg )
init state =
  let
    decodedState = Json.Decode.decodeValue decodeState state
    finalState = case decodedState of
      Ok s -> s
      Err _ ->
        { lighting = "light"
        , vertName = "Us"
        , horzName = "Them"
        , vertScore = 0
        , horzScore = 0
        , history = [(0,0)]
        }
  in
  ( modelFromState finalState
  , Cmd.none
  )



-- UPDATE

type Msg
  = ChangePlayerBet Player Bet Bool
  | ChangeFirstOut Player Bool
  | ChangeTeamScore String
  | ChangeTeamName Team String
  | ConsecutiveVictory Team Bool
  | CrashApp
  | Score
  | Clear
  | Undo
  | ToggleSettings Bool
  | Update
  | ChangeLighting Bool
  | ShowConfirmation String Msg
  | CloseConfirmation
  | UpdateAvailable


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePlayerBet playerType bet checked ->
      ( { model | scorer = changePlayerBet model.scorer playerType (if checked then bet else Zero) }, Cmd.none )
    ChangeFirstOut playerType result ->
      ( { model | scorer = changeFirstOut model.scorer playerType result }, Cmd.none )
    Score ->
      ( { model | scorer = scoreAll model.scorer }, Cmd.none )
    ChangeTeamScore val ->
      ( case String.toInt val of
          Nothing -> model
          Just s -> { model | scorer = (changeTurnScore model.scorer s) }
      , Cmd.none
      )
    ChangeTeamName team name ->
      ( changeTeamName model team name, Cmd.none )
    ConsecutiveVictory team result ->
      ( { model | scorer = consecutiveVictory model.scorer team result }, Cmd.none )
    CrashApp ->
      let
        resetModel = defaultModel model.lighting model.vertName model.horzName
      in
        ( { resetModel | crashed = True }, Cmd.none )
    Clear ->
      ( defaultModel model.lighting model.vertName model.horzName, Cmd.none )
    Undo ->
      ( { model | scorer = undo model.scorer }, Cmd.none )
    Update ->
      ( model, Browser.Navigation.reloadAndSkipCache )
    ChangeLighting checked ->
      ( { model | lighting = if checked then Light else Dark }, Cmd.none )
    ToggleSettings checked ->
      ( { model | showSettings = checked }, Cmd.none )
    ShowConfirmation query confirmMsg ->
      ( { model | confirm = Active query confirmMsg }, Cmd.none )
    CloseConfirmation ->
      ( { model | confirm = Hidden }, Cmd.none )
    UpdateAvailable ->
      ( { model | updateAvailable = True }, Cmd.none )


changeTeamName : Model -> Team -> String -> Model
changeTeamName model team name =
  case team of
    Vertical -> { model | vertName = name }
    Horizontal -> { model | horzName = name }  



-- VIEW

view : Model -> Html Msg
view model =
  if model.crashed then
    text "The app crashed :("
  else 
    div [ class ("app " ++ if model.lighting == Light then "light" else "dark" ) ]
      [ div [ class "safe-area" ]
        [ viewScorer model
        , if model.showSettings then
            shield (ToggleSettings False) False
          else
            text ""
        , confirm model 
        , div [ class ("settings-container" ++ (if model.updateAvailable then " avail" else "")) ]
          [ if model.showSettings then
              viewSettings model
            else
              text ""
          , labeledCheckbox
              "settings-toggle"
              "Settings"
              "settings-toggle"
              "settings-label"
              model.showSettings
              ToggleSettings
          ]
        ]
      ]


viewScorer : Model -> Html Msg
viewScorer model =
  div [ class "scorer" ] 
    [ viewTeams model
    , viewTeamTurnScore model.scorer
    , viewConsecutiveVictory model.scorer
    , viewActions model
    , if (abs (model.scorer.vertScore - model.scorer.horzScore)) > 400 then
        button [ class "uh-oh", onClick CrashApp ] [ text "Things are not looking good" ]
      else
        text ""
    ]


viewTeams : Model -> Html Msg
viewTeams model =
  div [ class "teams" ]
    [ viewTeam model Vertical model.vertName model.scorer.vertScore model.scorer.north model.scorer.south
    , viewTeam model Horizontal model.horzName model.scorer.horzScore model.scorer.east model.scorer.west
    ]


viewTeam : Model -> Team -> String -> Int -> (Player, Bet) -> (Player, Bet) -> Html Msg
viewTeam model team name score player1 player2 =
  let
    colors = colorValues model.lighting
  in
  div [ class "team" ]
    [ input
      [ type_ "text"
      , Html.Styled.Attributes.value name
      , onInput (ChangeTeamName team)
      , css
          [ textAlign center
          , border zero
          , fontSize (px 20)
          , marginTop (px 20)
          , marginLeft auto
          , marginRight auto
          , displayFlex
          , paddingBottom (px 10)
          , Css.width (pct 90)
          , focus [ outline none ]
          , backgroundColor transparent
          , color colors.text
          ]
      ]
      []
    , div [ class "team-score"] [ text (String.fromInt score) ]
    , viewPlayer model.scorer player1
    , viewPlayer model.scorer player2
    ]


viewPlayer : Scorer -> (Player, Bet) -> Html Msg
viewPlayer model playerBet =
  viewPlayerBet model playerBet


viewPlayerBet : Scorer -> (Player, Bet) -> Html Msg
viewPlayerBet model (player, bet) =
  let
    playerId = getPlayerId player
    successful = bet /= Zero
      && (case model.firstOut of
            One p -> p == player
            Team (_, Just p) -> p == player
            _ -> False 
          )
  in  
    div [ class "bets" ]
      [ div [ class "bet" ]
          [ labeledCheckbox
              ("tichu" ++ "-" ++ playerId)
              "Tichu"
              "bet-radio"
              "bet-label"
              (bet == Tichu)
              (ChangePlayerBet player Tichu)
          , labeledCheckbox
              ("grand" ++ "-" ++ playerId)
              "Grand"
              "bet-radio"
              "bet-label"
              (bet == GrandTichu)
              (ChangePlayerBet player GrandTichu)
          , if bet /= Zero then
              div [ class "success"]
                [ input
                [ type_ "checkbox"
                , class "bet-success"
                , id ("success" ++ "-" ++ playerId)
                , Html.Styled.Attributes.checked successful
                , onCheck (ChangeFirstOut player) ] []
                , label
                  [ class ("bet-success-label" ++ if successful then " successful" else "" )
                  , for ("success" ++ "-" ++ playerId)
                  ]
                  [ text (if successful then "✓" else "✗") ]
                ]
            else
              text ""
          ]
      ]


viewTeamTurnScore : Scorer -> Html Msg
viewTeamTurnScore model =
  div [ class "turn-scores", css [ displayFlex, flexDirection row ] ]
    [ button
        [ class "score-step"
        , onClick (ChangeTeamScore (String.fromInt (model.vertTurnScore - 5)))
        ]
        [ text "<" ]
    , div [ class "turn-score vert" ] [ text (String.fromInt model.vertTurnScore) ]
    , input
      [ type_ "range"
      , class "range"
      , Html.Styled.Attributes.min "-25"
      , Html.Styled.Attributes.max "125"
      , Html.Styled.Attributes.value (String.fromInt model.vertTurnScore)
      , step "5"
      , onInput ChangeTeamScore ] []
    , div [ class "turn-score horz" ] [ text (String.fromInt (100 - model.vertTurnScore)) ]
    , button
        [ class "score-step"
        , onClick (ChangeTeamScore (String.fromInt (model.vertTurnScore + 5)))
        ]
        [ text ">" ]
    ]


viewConsecutiveVictory : Scorer -> Html Msg
viewConsecutiveVictory model =
  div [ class "consecutives", css [ displayFlex, flexDirection row ] ]
    [ div [ class "consecutive"] 
      [ input
        [ type_ "checkbox"
        , class "cv-check"
        , id "vert-cv"
        , Html.Styled.Attributes.checked
          (case model.firstOut of
            Team (Vertical, _) -> True      
            _ -> False
          )
        , onCheck (ConsecutiveVictory Vertical)
        ][]
      , label [ class "cv-label", for "vert-cv" ] [ text "Consecutive" ]
      ]
    , div [ class "consecutive" ] 
      [ input
        [ type_ "checkbox"
        , class "cv-check"
        , id "horz-cv"
        , Html.Styled.Attributes.checked
          (case model.firstOut of
            Team (Horizontal, _) -> True      
            _ -> False
          )
        , onCheck (ConsecutiveVictory Horizontal)
        ][]
      , label [ class "cv-label", for "horz-cv" ] [ text "Consecutive" ]
      ]
    ]


viewActions : Model -> Html Msg
viewActions model =
  div [ class "view-actions" ]
    [ button [ class "undo", onClick Undo ] [ text "Undo" ]
    , button [ class "score", onClick Score ] [ text "Score" ]
    , button [ class "clear", onClick (ShowConfirmation "Are you sure you want to reset?" Clear) ] [ text "Reset" ]
    ]


viewSettings : Model -> Html Msg
viewSettings model =
  div [ class "settings" ]
    [ labeledCheckbox
        "lighting"
        (if model.lighting == Light then "Dark mode" else "Light mode")
        "lighting"
        "lighting-label"
        (model.lighting == Light)
        ChangeLighting
    , hr [] []
    , button [ class "update", onClick Update ] [ text (if model.updateAvailable then "Update" else "Reload") ]
    ]


labeledRadio : String -> String -> String -> String -> String -> Bool -> (String -> Msg) -> Html Msg
labeledRadio elemid elemlabel rgroup elemclass labelclass isChecked msg =
  div []
    [ input 
      [ type_ "radio"
      , class elemclass
      , id elemid
      , name rgroup
      , Html.Styled.Attributes.checked isChecked
      , onInput msg
      ] []
    , label [ class labelclass, for elemid ] [ text elemlabel ]
    ]

labeledCheckbox : String -> String -> String -> String -> Bool -> (Bool -> Msg) -> Html Msg
labeledCheckbox elemid elemlabel elemclass labelclass isChecked msg =
  div [] 
    [ input
      [ type_ "checkbox"
      , id elemid
      , class elemclass
      , Html.Styled.Attributes.checked isChecked
      , onCheck msg
      ] []
    , label [ class labelclass, for elemid ] [ text elemlabel ]
    ]


confirm : Model -> Html Msg
confirm model =
  let
    colors = colorValues model.lighting
  in
  case model.confirm of
    Hidden -> text ""
    Active query msg ->
      div
        []
        [ shield CloseConfirmation True
        , div
          [ css
            [ position absolute
            , left (pct 50)
            , top (pct 50)
            , transform (translate2 (pct -50) (pct -66))
            , minWidth maxContent
            ]
          ]
          [
            div 
            [ css
              [ backgroundColor colors.background
              , borderRadius (px 30)
              , border3 (px 1) solid colors.border
              , padding (px 20)
              ]
            ]
            [ div
              [ css
                [ textAlign center
                , marginTop (px 25)
                , marginBottom (px 45)
                ]
              ]
              [ text query ]
            , div
              [ css
                [ displayFlex
                ]
              ]
              [ button
                [ onClick CloseConfirmation
                , css [ confirmButtonStyle ]
                ]
                [ text "No" ]
              , button
                [ onClick msg
                , css
                  [ confirmButtonStyle
                  , ctaStyle
                  , marginLeft auto
                  ]
                ]
                [ text "Yes" ]
              ]
            ]
          ]
        ]


shield : Msg -> Bool -> Html Msg
shield msg dim =
  div 
    [ css
      [ position absolute
      , left (px 0)
      , top (px 0)
      , right (px 0)
      , bottom (px 0)
      , if dim then
          batch
          [ backgroundColor (hex "#111")
          , opacity (num 0.2)
          ]
        else
          batch []
      ]
    , onClick msg
    ]
    []


type alias Colors =
  { border: Color
  , background: Color
  , menuBackground: Color
  , cta: Color
  , ctaText: Color
  , text: Color
  }


colorValues : Lighting -> Colors
colorValues lighting =
  if lighting == Dark then
    { border = (hex "333")
    , background = (hex "000")
    , menuBackground = (hex "111")
    , text = (hex "CCC")
    , cta = (hex "DBB004")
    , ctaText = (hex "000")
    }
  else
    { border = (hex "CCC")
    , background = (hex "FFF")
    , menuBackground = (hex "EEE")
    , cta = (hex "DBB004")
    , ctaText = (hex "000")
    , text = (hex "000")
    }


confirmButtonStyle : Style
confirmButtonStyle =
  batch
    [ borderRadius (px 10)
    , Css.width (pct 45)
    , Css.height (px 40)
    ]


ctaStyle : Style
ctaStyle = 
 batch
  [ important (color (hex "000"))
  , important (backgroundColor (hex "DBB004"))
  ]


{-| Decodes two fields into a tuple.
- https://stackoverflow.com/a/53017452
-}
decodeAsTuple2 : String -> Json.Decode.Decoder a -> String -> Json.Decode.Decoder b -> Json.Decode.Decoder (a, b)
decodeAsTuple2 fieldA decoderA fieldB decoderB =
    let
        result : a -> b -> (a, b)
        result valueA valueB =
            (valueA, valueB)
    in
        Json.Decode.succeed result
            |> Json.Decode.Extra.andMap (Json.Decode.field fieldA decoderA)
            |> Json.Decode.Extra.andMap (Json.Decode.field fieldB decoderB)
