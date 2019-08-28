port module Main exposing (..)

import Browser
import Browser.Navigation
import Css exposing (..)
import Dict exposing (Dict)
import HtmlHelper exposing (hr, range)
import Html.Styled exposing (Html, div, text, button, input, label, li, toUnstyled)
import Html.Styled.Attributes exposing
  ( id, type_, class, css, for, name, value, checked)
import Html.Styled.Events exposing (onInput, onClick, onCheck)
import Http
import Json.Decode exposing
  (Decoder, Value, decodeValue, succeed, map6, field, string, int, list)
import Json.Decode.Extra exposing (andMap)
import Scorer exposing (..)
import Svgs exposing (consecutiveVictorySvg, undoSvg)
import Time exposing (Posix, every)
import Theme exposing (ThemeSettings, light, dark, glitter)
import Version exposing (Version, versionDecoder, compareVersion)


-- MAIN

main : Program Json.Decode.Value Model Msg
main =
  Browser.document
    { init = init
    , view = \model -> { title = "Tichu 3", body = [ model |> view |> toUnstyled ] }
    , update = updateWithStorage
    , subscriptions = subscriptions
    }


port setStorage : State -> Cmd msg

port updateAvailable : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
  [ updateAvailable (\_ -> UpdateAvailable)
  , every (5 * 1000) CheckForUpdate
  ]


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
  , settingBet: SettingBet
  , vertName: String
  , horzName: String
  , theme: ThemeSettings
  , themes: Dict String ThemeSettings
  , showSettings: Bool
  , changingTheme: Bool
  , updateAvailable: Bool
  , checkingForUpdate: Bool
  , currentVersion: String
  , foundVersion: String
  , crashed: Bool
  , confirm: Confirm
  }


defaultModel : ThemeSettings -> String -> String -> Model
defaultModel theme vertName horzName =
  { scorer = defaultScorer
  , settingBet = NoOne
  , vertName = vertName
  , horzName = horzName
  , theme = theme
  , themes = themes
  , showSettings = False
  , changingTheme = False
  , checkingForUpdate = False
  , currentVersion = "0.0.0"
  , foundVersion = "0.0.0"
  , updateAvailable = False
  , crashed = False
  , confirm = Hidden
  }


themes : Dict String ThemeSettings
themes = Dict.fromList (List.map (\t -> (t.id, t)) [ dark, glitter, light ])


modelFromState : State -> Model
modelFromState state =
  let
    model = defaultModel
      ( case Dict.get state.lighting themes of
          Just t -> t
          Nothing -> light
      )
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
    (field "vertScore" int)
    (field "horzScore" int)
    (field "history" (list (decodeAsTuple2 "0" int "1" int)))


getState : Model -> State
getState model =
  { lighting = model.theme.id
  , vertName = model.vertName
  , horzName = model.horzName
  , vertScore = model.scorer.vertScore
  , horzScore = model.scorer.horzScore
  , history = model.scorer.history
  }


type Confirm
  = Hidden
  | Active String Msg


type SettingBet
  = NoOne
  | Person Player


init : Value -> ( Model, Cmd Msg )
init state =
  let
    decodedState = decodeValue decodeState state
    finalState = case decodedState of
      Ok s -> s
      Err _ ->
        { lighting = light.id
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
  | ChangeSettingBet Player
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
  | ChangeLighting String
  | ShowConfirmation String Msg
  | ChangingTheme Bool
  | CloseConfirmation
  | CheckForUpdate Posix
  | CheckedVersion (Result Http.Error Version)
  | CheckedCurrentVersion Version (Result Http.Error Version)
  | UpdateAvailable


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePlayerBet playerType bet checked ->
      ( { model | scorer = changePlayerBet model.scorer playerType (if checked then bet else Zero), settingBet = NoOne }, Cmd.none )
    ChangeSettingBet player ->
      ( { model | settingBet = Person player }, Cmd.none )
    ChangeFirstOut playerType result ->
      ( { model | scorer = changeFirstOut model.scorer playerType result }, Cmd.none )
    Score ->
      ( { model | scorer = scoreAll model.scorer }, Cmd.none )
    ChangeTeamScore val ->
      ( case String.toInt val of
          Nothing -> model
          Just s -> { model | scorer = changeTurnScore model.scorer s }
      , Cmd.none
      )
    ChangeTeamName team name ->
      ( changeTeamName model team name, Cmd.none )
    ConsecutiveVictory team result ->
      ( { model | scorer = consecutiveVictory model.scorer team result }, Cmd.none )
    CrashApp ->
      let
        resetModel = defaultModel model.theme model.vertName model.horzName
      in
        ( { resetModel | crashed = True }, Cmd.none )
    Clear ->
      ( defaultModel model.theme model.vertName model.horzName, Cmd.none )
    Undo ->
      ( { model | scorer = undo model.scorer }, Cmd.none )
    Update ->
      ( model, Browser.Navigation.reloadAndSkipCache )
    ChangeLighting id ->
      ( changeTheme model id, Cmd.none )
    ToggleSettings checked ->
      ( { model | showSettings = checked, changingTheme = False }, Cmd.none )
    ChangingTheme checked ->
      ( { model | changingTheme = checked }, Cmd.none )
    ShowConfirmation query confirmMsg ->
      ( { model | confirm = Active query confirmMsg }, Cmd.none )
    CloseConfirmation ->
      ( { model | confirm = Hidden }, Cmd.none )
    CheckForUpdate _ -> ( model, checkForUpdate )
    CheckedVersion versionResult ->
      case versionResult of
        Ok version -> ( { model | foundVersion = version.version }, getCurrentVersion version )
        Err _ -> ( model, Cmd.none )
    CheckedCurrentVersion theirs oursResult ->
      case oursResult of
        Ok ours -> ( { model | currentVersion = ours.version, updateAvailable = compareVersion ours theirs }, Cmd.none )
        Err _ -> ( model, Cmd.none )
    UpdateAvailable ->
      ( { model | updateAvailable = True }, Cmd.none )


changeTeamName : Model -> Team -> String -> Model
changeTeamName model team name =
  case team of
    Vertical -> { model | vertName = name }
    Horizontal -> { model | horzName = name }


changeTheme : Model -> String -> Model
changeTheme model id =
  let
    maybeTheme = Dict.get id model.themes
  in
    case maybeTheme of
      Just theme -> { model | theme = theme }
      Nothing -> model


-- VIEW

view : Model -> Html Msg
view model =
  if model.crashed then
    div
      [ css [ property "margin-top" "env(safe-area-inset-top)" ] ]
      [ text "The app crashed :(" ]
  else 
    div
      [ css
        [ width (pct 100)
        , height (pct 100)
        , position fixed
        , property "user-select" "none"
        , property "-moz-user-select" "none"
        , property "-webkit-user-select" "none"
        , property "-ms-user-select" "none"
        , backgroundColor model.theme.colors.background
        , color model.theme.colors.text
        , fontFamilies
          [ "-apple-system"
          , "BlinkMacSystemFont"
          , qt "Segoe UI"
          , "Roboto"
          , "Helvetica"
          , "Arial"
          , "san-serif"
          , qt "Apple Color Emoji"
          , qt "Segoe UI Emoji"
          , qt "Segoe UI Symbol"
          ]
        ]
      ]
      [ div
        [ css
          [ width (pct 100)
          , height (pct 100)
          , boxSizing borderBox
          , property "padding-left" "env(safe-area-inset-left)"
          , property "padding-right" "env(safe-area-inset-right)"
          , property "padding-top" "env(safe-area-inset-top)"
          , property "padding-bottom" "env(safe-area-inset-bottom)"
          ]
        ]
        [ viewScorer model
        , if model.checkingForUpdate then text "Checking for update..." else text ""
        , text ("Current version: " ++ model.currentVersion)
        , text ("Found version:  " ++ model.foundVersion)
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
  div
    [ css
      [ property "display" "grid"
      , property "grid-row-gap" "20px"
      ]
    ] 
    [ viewTeams model
    , viewTurnScores model
    , viewBetRow model
    , viewActions model
    , if abs (model.scorer.vertScore - model.scorer.horzScore) > 400 then
        button [ class "uh-oh", onClick CrashApp ] [ text "Things are not looking good" ]
      else
        text ""
    ]


viewTeams : Model -> Html Msg
viewTeams model =
  div
    [ css
      [ displayFlex
      , property "justify-content" "space-evenly"
      ]
    ]
    [ viewTeam model Vertical model.vertName model.scorer.vertScore
    , viewTeam model Horizontal model.horzName model.scorer.horzScore
    ]


viewTeam : Model -> Team -> String -> Int -> Html Msg
viewTeam model team name score =
  div
    [ css
      [ border3 (px 3) solid model.theme.colors.border
      , borderRadius (px 10)
      , marginTop (px 20)
      , width (px 90)
      ]
    ]
    [ div
      [ css
        [ padding2 (px 5) (px 5)
        , borderTopRightRadius (px 7)
        , borderTopLeftRadius (px 7)
        , backgroundColor model.theme.colors.menuBackground
        ]
      ]
      [ input
        [ type_ "text"
        , value name
        , onInput (ChangeTeamName team)
        , css
            [ textAlign center
            , border zero
            , fontSize (px 15)
            , marginLeft auto
            , marginRight auto
            , displayFlex
            , width (pct 90)
            , focus [ outline none ]
            , backgroundColor transparent
            , color model.theme.colors.text
            , textAlign center
            ]
        ]
        []
      ]
    , div
      [ css
        [ textAlign center
        , fontSize (px 35)
        , padding (px 5)
        ]
      ]
      [ text (String.fromInt score) ]
    ]


viewBetRow : Model -> Html Msg
viewBetRow model =
  div
    [ css
      [ marginBottom (px 10)
      , border3 (px 2) solid model.theme.colors.border
      , borderRadius (px 20)
      , padding (px 10)
      , displayFlex
      , alignItems center
      , margin auto
      , maxWidth maxContent
      ]
    ]
    ( case model.settingBet of
        Person player ->
          [ div
            [ onClick (ChangePlayerBet player Tichu True)
            , css
              [ cursor pointer
              , borderRight3 (px 2) solid model.theme.colors.border
              , display inlineBlock
              , paddingRight (px 10)
              , marginRight (px 10)
              ]
            ]
            [ text "Tichu" ]
            , div
              [ onClick (ChangePlayerBet player GrandTichu True)
              , css
                [ cursor pointer
                ]
              ]
              [ text "Grand"]
          ]
        NoOne ->
          [ div
            [ css
              [ borderRight3 (px 2) solid model.theme.colors.border
              , display inlineBlock
              , paddingRight (px 10)
              , marginRight (px 10)
              ]
            ] [ viewBets model model.scorer.north model.scorer.south ]
          , viewBets model model.scorer.west model.scorer.east
          ]
    )

viewBets : Model -> (Player, Bet) -> (Player, Bet) -> Html Msg
viewBets model (player1, bet1) (player2, bet2) =
  div
    [ css
      [ position relative
      ]
    ]
    [ viewAddBet model (player1, bet1)
    , (if bet1 /= Zero then
      ( if bet2 == Zero then
          viewAddBetMin model (player2, bet2)
        else
          viewAddBet model (player2, bet2)
      )
      else
        text "")
    ]


viewAddBet : Model -> (Player, Bet) -> Html Msg
viewAddBet model (player, bet) =
  div
    [ css
      [ position relative
      , nthChild "2" [ marginTop (px 10) ]
      ]
    ]
    [ if bet == Zero then
        div
          [ css
            [ padding2 (px 6) (px 10)
            , border3 (px 2) solid model.theme.colors.border
            , borderRadius (px 10)
            , width (px 94)
            , boxSizing borderBox
            , textAlign center
            , cursor pointer
            ]
          , onClick (ChangeSettingBet player)
          ]
          [ text "+ Bet" ]
      else
        viewBet model (player, bet)
    ]


viewAddBetMin : Model -> (Player, Bet) -> Html Msg
viewAddBetMin model (player, bet) =
  div
    [ css
      [ position absolute
      , backgroundColor model.theme.colors.background
      , left (pct 50)
      , transform (translateX (pct -50))
      , padding2 (px 1) (px 10)
      , borderBottom3 (px 2) solid model.theme.colors.border
      , borderBottomLeftRadius (px 10)
      , borderBottomRightRadius (px 10)
      , borderTop zero
      , boxSizing borderBox
      , textAlign center
      , lineHeight initial
      , cursor pointer
      ]
    , onClick (ChangeSettingBet player)
    ]
    [ text "+" ]


viewBet : Model -> (Player, Bet) -> Html Msg
viewBet model (player, bet) =
  let
    betLabel = if bet == Tichu then "Tichu" else "Grand"
    playerId = getPlayerId player
    successful = bet /= Zero
      && (case model.scorer.firstOut of
            One p -> p == player
            Team (_, Just p) -> p == player
            _ -> False)
  in
    div
      [ css
        [ displayFlex
        , alignItems center
        , border3 (px 2) solid transparent
        ]
      ]
      [ div
        [ css
          [ borderRight3 (px 1) solid model.theme.colors.border
          , padding2 (px 6) (px 10)
          , cursor pointer
          , backgroundColor model.theme.colors.border
          , borderTopLeftRadius (px 10)
          , borderBottomLeftRadius (px 10)
          ]
        , onClick (ChangePlayerBet player bet False)
        ]
        [ text "x" ]
      , div
        [ css
          []
        ]
        [ input
          [ type_ "checkbox"
          , id ("success" ++ "-" ++ playerId)
          , checked successful
          , onCheck (ChangeFirstOut player)
          , css
            [ display none
            ]
          ]
          []
        , label
          [ for ("success" ++ "-" ++ playerId)
          , css
            [ padding2 (px 6) (px 10)
            , cursor pointer
            , borderTopRightRadius (px 10)
            , borderBottomRightRadius (px 10)
            , backgroundColor (hex (if successful then "71be44" else "be6044"))
            ]
          ]
          [ text betLabel ]
        ]
      ]


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
    div
      [ css
        [
        ]
      ]
      [ div
        [ css
          [
          ]
        ]
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
              , checked successful
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


viewTurnScores : Model -> Html Msg
viewTurnScores model =
  div
    [ css
      [ displayFlex
      , flexDirection column
      ]
    ]
    [ viewTeamTurnScores model
    , viewTeamTurnScoreSlider model
    ]


viewTeamTurnScores : Model -> Html Msg
viewTeamTurnScores model =
  div
    [ css
      [ margin auto
      ]
    ]
    [ viewTeamTurnScore model model.scorer.vertTurnScore Vertical
    , viewTeamTurnScore model (100 - model.scorer.vertTurnScore) Horizontal
    ]


viewTeamTurnScore : Model -> Int -> Team -> Html Msg
viewTeamTurnScore model teamScore team =
  let
    widthPx = 60
  in
  div
    [ css
      [ display inlineBlock
      ]
    ]
    [ div
        [ css
          [ height (px 25)
          , width (px widthPx)
          , padding2 zero (px 10)
          , backgroundColor model.theme.colors.menuBackground
          , border3 (px 3) solid model.theme.colors.border
          , borderBottom zero
          , boxSizing borderBox
          , borderRadius zero
          , fontWeight bold
          , case team of
              -- Left
              Vertical ->
                batch
                  [ borderTopLeftRadius (px 25)
                  , borderRight zero
                  , width (px (widthPx - 1))
                  , textAlign right
                  ]
              -- Right
              Horizontal ->
                batch
                  [ borderTopRightRadius (px 25)
                  , textAlign left
                  ]
          ]
        ]
        [ div
            [ css
              [ top (pct 50)
              , transform (translateY (pct -50))
              , position relative
              ]
            ]
            [ text (String.fromInt teamScore) ]
        ]
    ]


viewConsecutiveVictoryButton : Model -> String -> Team -> Bool -> Html Msg
viewConsecutiveVictoryButton model elemid team ischecked =
  div
    [ css []
    ] 
    [ input
      [ type_ "checkbox"
      , id elemid
      , checked ischecked
      , onCheck (ConsecutiveVictory team)
      , css
        [ display none
        ]
      ][]
    , label
        [ for elemid
        , css
          [ border3 (px 3) solid model.theme.colors.border
          , width (px 50)
          , height (px 50)
          , display inlineBlock
          , boxSizing borderBox
          , padding2 zero (px 7)
          , backgroundColor model.theme.colors.border
          , cursor pointer
          , case team of
              -- Left
              Vertical ->
                batch
                  [ borderTopLeftRadius (px 25)
                  , borderBottomLeftRadius (px 25)
                  , paddingLeft (px 16)
                  , textAlign right
                  ]
              -- Right
              Horizontal ->
                batch
                  [ borderTopRightRadius (px 25)
                  , borderBottomRightRadius (px 25)
                  , textAlign left
                  ]
          , if ischecked then batch
              [ backgroundColor model.theme.colors.cta
              , borderColor model.theme.colors.cta
              ]
            else
              batch []
          ]
        ]
        [ div
          [ css
            [ top (pct 50)
            , transform (translateY (pct -50))
            , position relative
            , width (px 22)
            , height (px 22)
            ]
          ]
          [ consecutiveVictorySvg ]
        ]
    ]


viewTeamTurnScoreSlider : Model -> Html Msg
viewTeamTurnScoreSlider model =
  div
    [ css
      [ displayFlex
      , flexDirection row
      , justifyContent center
      ]
    ]
    [ viewConsecutiveVictoryButton
      model
      "vert-cv"
      Vertical        
      (case model.scorer.firstOut of
        Team (Vertical, _) -> True      
        _ -> False
      )
    , case model.scorer.firstOut of 
        Team t -> 
          div
            [ css
              [ width (px 240)
              , height (px 50)
              , lineHeight (px 50)
              , backgroundColor model.theme.colors.cta
              , color (hex "000")
              , cursor pointer
              , batch (case t of
                (Horizontal, _) ->
                  [ textAlign right
                  ]
                _ -> []
              )
              ]
            , onClick (ConsecutiveVictory Vertical False)
            ]
            [ text "Consecutive victory" ]
        _ -> viewSlider model
    , viewConsecutiveVictoryButton
      model
      "horz-cv"
      Horizontal
      (case model.scorer.firstOut of
        Team (Horizontal, _) -> True      
        _ -> False
      )
    ]


viewSlider : Model -> Html Msg
viewSlider model =
  range
    {
      min = -25
    , max = 125
    , step = 5
    , value = model.scorer.vertTurnScore
    , onInput = ChangeTeamScore
    }
    { width = 250
    , height = 50
    , padding = 5
    , background = model.theme.colors.background
    , trackBorderRadius = 0
    , thumbHeight = 30
    , thumbWidth = 40
    , thumbColor = model.theme.colors.pop
    , thumbBorderRadius = 20
    , additional =
      [ borderTop3 (px 3) solid model.theme.colors.border
      , borderBottom3 (px 3) solid model.theme.colors.border
      ]
    }


viewActions : Model -> Html Msg
viewActions model =
  div
    [ css
      [ displayFlex
      , alignItems center
      ]
    ]
    [ div
      [ css
        [ width (px 45)
        ]
      ]
      []
    , button
      [ css
        [ width (px 150)
        , padding2 (px 10) zero
        , backgroundColor model.theme.colors.cta
        , color model.theme.colors.ctaText
        , borderRadius (px 10)
        , marginLeft auto
        ]
      , onClick Score
      ]
      [ text "Score" ]
    , div
      [ onClick Undo
      , css
        [ width (px 25)
        , height (px 25)
        , cursor pointer
        , marginLeft (px 20)
        , marginRight auto
        ]
      ]
      [ undoSvg ]
    ]


viewSettings : Model -> Html Msg
viewSettings model =
  div
    [ css
      [ border3 (px 3) solid model.theme.colors.border
      , borderRadius (px 20)
      , padding (px 10)
      , backgroundColor model.theme.colors.background
      , width (px 150)
      , textAlign left
      , marginBottom (px 10)
      ]
    ]
    (if model.changingTheme then themeSettings model else defaultSettings model)


defaultSettings : Model -> List (Html Msg)
defaultSettings model =
  let
    buttonStyles =
      [ cursor pointer
      , width (pct 100)
      , padding2 (px 6) zero
      , backgroundColor model.theme.colors.menuBackground
      , color inherit
      , borderRadius (px 10)
      ]
  in
  [ button
    [ onClick (ChangingTheme True)
    , css
      [ batch buttonStyles
      ]
    ]
    [ text "Change theme"]
  , button
    [ css
      [ batch buttonStyles
      , marginTop (px 10)
      ]
    , onClick (ShowConfirmation "Are you sure you want to reset?" Clear)
    ]
    [ text "Reset" ]
  , hr 2 model.theme.colors.border [ margin2 (px 10) zero ]
  , button
    [ css
      [ batch buttonStyles
      ]
    , onClick Update
    ]
    [ text (if model.updateAvailable then "Update" else "Reload") ]
  ]


themeSettings : Model -> List (Html Msg)
themeSettings model =
  [ div
    [ css
      [ paddingBottom (px 10)
      , marginBottom (px 10)
      , fontWeight bold
      , cursor pointer
      , borderBottom3 (px 2) solid model.theme.colors.border
      ]
    , onClick (ChangingTheme False)
    ]
    [ text "< Theme" ]
  , div
    [
    ]
    (List.map
      (\theme ->
        li
          [ onClick (ChangeLighting theme.id)
          , css
            [ batch (if model.theme.id == theme.id then [ fontWeight bold ] else [])
            , marginBottom (px 10)
            , cursor pointer
            , lastChild [ marginBottom zero ]
            ]
          ]
          [ text theme.name
          ]
      )
      (Dict.values themes))
  ]

labeledCheckbox : String -> String -> String -> String -> Bool -> (Bool -> Msg) -> Html Msg
labeledCheckbox elemid elemlabel elemclass labelclass isChecked msg =
  div [] 
    [ input
      [ type_ "checkbox"
      , id elemid
      , class elemclass
      , checked isChecked
      , onCheck msg
      ] []
    , label [ class labelclass, for elemid ] [ text elemlabel ]
    ]


confirm : Model -> Html Msg
confirm model =
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
              [ backgroundColor model.theme.colors.background
              , borderRadius (px 30)
              , border3 (px 1) solid model.theme.colors.border
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
                  , backgroundColor model.theme.colors.cta
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


confirmButtonStyle : Style
confirmButtonStyle =
  batch
    [ borderRadius (px 10)
    , width (pct 45)
    , height (px 40)
    ]


{-| Decodes two fields into a tuple.
- https://stackoverflow.com/a/53017452
-}
decodeAsTuple2 : String -> Decoder a -> String -> Decoder b -> Decoder (a, b)
decodeAsTuple2 fieldA decoderA fieldB decoderB =
    let
        result : a -> b -> (a, b)
        result valueA valueB =
            (valueA, valueB)
    in
        succeed result
            |> andMap (field fieldA decoderA)
            |> andMap (field fieldB decoderB)


-- HTTP


checkForUpdate : Cmd Msg
checkForUpdate =
  Http.get
    { url = "https://tichu.netlify.com/version.json"
    , expect = Http.expectJson CheckedVersion versionDecoder
    }


getCurrentVersion : Version -> Cmd Msg
getCurrentVersion version =
  Http.get
    { url = "/version.json"
    , expect = Http.expectJson (CheckedCurrentVersion version) versionDecoder
    }

