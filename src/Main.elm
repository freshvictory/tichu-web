port module Main exposing (..)

import Browser
import Browser.Navigation
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Dict exposing (Dict)
import Html.Styled exposing (Html, a, button, div, input, label, li, span, text, toUnstyled)
import Html.Styled.Attributes exposing (checked, css, for, href, id, target, title, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput)
import HtmlHelper exposing (hr, range, neumorphicShadow)
import Http
import Json.Decode exposing (Decoder, Value, decodeValue, field, int, list, map6, string, succeed)
import Json.Decode.Extra exposing (andMap)
import Scorer exposing (..)
import Svgs exposing (consecutiveVictorySvg, gearSvg, trashSvg, undoSvg, xSvg)
import Theme exposing (ThemeSettings, dark, light, mint, strawberry)
import Time exposing (Posix, every)
import Version exposing (Version, compareVersion, versionDecoder)



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Tichu Beta", body = [ model |> view |> toUnstyled ] }
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


port setStorage : State -> Cmd msg


port updateAvailable : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ updateAvailable (\_ -> UpdateAvailable)
        , every (5 * 60 * 1000) CheckForUpdate
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
    { scorer : Scorer
    , settingBet : SettingBet
    , vertName : String
    , horzName : String
    , theme : ThemeSettings
    , themes : Dict String ThemeSettings
    , showSettings : Bool
    , updateAvailable : Bool
    , checkingForUpdate : Bool
    , currentVersion : Version
    , foundVersion : Version
    , crashed : Bool
    , confirm : Confirm
    , showAbout : Bool
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
    , checkingForUpdate = False
    , currentVersion = { version = "1.2.4" }
    , foundVersion = { version = "0.0.0" }
    , updateAvailable = False
    , crashed = False
    , confirm = Hidden
    , showAbout = False
    }


themes : Dict String ThemeSettings
themes =
    Dict.fromList (List.map (\t -> ( t.id, t )) [ dark, strawberry, light, mint ])


modelFromState : State -> Model
modelFromState state =
    let
        model =
            defaultModel
                (case Dict.get state.lighting themes of
                    Just t ->
                        t

                    Nothing ->
                        light
                )
                state.vertName
                state.horzName

        scorer =
            model.scorer

        newScorer =
            { scorer
                | vertScore = state.vertScore
                , horzScore = state.horzScore
                , history = state.history
            }
    in
    { model | scorer = newScorer }


type alias State =
    { lighting : String
    , vertName : String
    , horzName : String
    , vertScore : Int
    , horzScore : Int
    , history : List ( Int, Int )
    }


decodeState : Decoder State
decodeState =
    Json.Decode.map6 State
        (field "lighting" string)
        (field "vertName" string)
        (field "horzName" string)
        (field "vertScore" Json.Decode.int)
        (field "horzScore" Json.Decode.int)
        (field "history" (list (decodeAsTuple2 "0" Json.Decode.int "1" Json.Decode.int)))


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
        decodedState =
            decodeValue decodeState state

        finalState =
            case decodedState of
                Ok s ->
                    s

                Err _ ->
                    { lighting = light.id
                    , vertName = "Us"
                    , horzName = "Them"
                    , vertScore = 0
                    , horzScore = 0
                    , history = [ ( 0, 0 ) ]
                    }
    in
    ( modelFromState finalState
    , checkForUpdate
    )



-- UPDATE


type Msg
    = ChangePlayerBet Player Bet Bool
    | ChangeSettingBet SettingBet
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
    | CloseConfirmation
    | CheckForUpdate Posix
    | CheckedVersion (Result Http.Error Version)
    | UpdateAvailable
    | ShowAbout Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePlayerBet playerType bet checked ->
            ( { model
                | scorer =
                    changePlayerBet model.scorer
                        playerType
                        (if checked then
                            bet

                         else
                            Zero
                        )
                , settingBet = NoOne
              }
            , Cmd.none
            )

        ChangeSettingBet player ->
            ( { model | settingBet = player }, Cmd.none )

        ChangeFirstOut playerType result ->
            ( { model | scorer = changeFirstOut model.scorer playerType result }, Cmd.none )

        Score ->
            ( { model | scorer = scoreAll model.scorer }, Cmd.none )

        ChangeTeamScore val ->
            ( case String.toInt val of
                Nothing ->
                    model

                Just s ->
                    { model | scorer = changeTurnScore model.scorer s }
            , Cmd.none
            )

        ChangeTeamName team name ->
            ( changeTeamName model team name, Cmd.none )

        ConsecutiveVictory team result ->
            ( { model | scorer = consecutiveVictory model.scorer team result }, Cmd.none )

        CrashApp ->
            let
                resetModel =
                    defaultModel model.theme model.vertName model.horzName
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
            ( { model | showSettings = checked, showAbout = False }, Cmd.none )

        ShowConfirmation query confirmMsg ->
            ( { model | confirm = Active query confirmMsg }, Cmd.none )

        CloseConfirmation ->
            ( { model | confirm = Hidden }, Cmd.none )

        CheckForUpdate _ ->
            ( model, checkForUpdate )

        CheckedVersion versionResult ->
            case versionResult of
                Ok version ->
                    ( { model | foundVersion = version, updateAvailable = compareVersion model.currentVersion version }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UpdateAvailable ->
            ( { model | updateAvailable = True }, Cmd.none )

        ShowAbout checked ->
            ( { model | showAbout = checked }, Cmd.none )


changeTeamName : Model -> Team -> String -> Model
changeTeamName model team name =
    case team of
        Vertical ->
            { model | vertName = name }

        Horizontal ->
            { model | horzName = name }


changeTheme : Model -> String -> Model
changeTheme model id =
    let
        maybeTheme =
            Dict.get id model.themes
    in
    case maybeTheme of
        Just theme ->
            { model | theme = theme }

        Nothing ->
            model



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
                , backgroundColor (hex model.theme.colors.background)
                , color (hex model.theme.colors.text)
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
                [ settingsGear model
                , viewScorer model
                , if model.showSettings then
                    shield (ToggleSettings False) False

                  else
                    text ""
                , div
                    [ css
                        [ position absolute
                        , bottom (px 30)
                        , right zero
                        , padding inherit
                        , margin (px 20)
                        , textAlign right
                        ]
                    ]
                    [ if model.showSettings then
                        viewSettings model

                      else
                        text ""
                    ]
                , confirm model
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
        , viewTurnScoreAndBets model
        , viewActions model
        , if abs (model.scorer.vertScore - model.scorer.horzScore) > 400 then
            button
                [ css
                    [ color (hex "FF0000")
                    , fontWeight bold
                    , border3 (px 1) solid (hex "FF0000")
                    , borderRadius (px 10)
                    , property "background" "repeating-linear-gradient(45deg,yellow,yellow 10px,black 10px,black 20px)"
                    , property "text-shadow" "-1px -1px 0 #000,1px -1px 0 #000,-1px 1px 0 #000,1px 1px 0 #000"
                    ]
                , onClick CrashApp
                ]
                [ text "Things are not looking good" ]

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
            [ border3 (px 0) solid (hex model.theme.colors.border)
            , borderRadius (px 10)
            , marginTop (px 20)
            , width (px 90)
            , overflow hidden
            , neumorphicShadow 5 10 model.theme.colors.lightShadow False
            ]
        ]
        [ div
            [ css
                [ padding2 (px 5) (px 5)
                , backgroundColor (hex model.theme.colors.menuBackground)
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
                    , display block -- Fix iOS centering
                    , width (pct 100)
                    , boxSizing borderBox
                    , focus [ outline none ]
                    , backgroundColor transparent
                    , color (hex model.theme.colors.text)
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


viewTurnScoreAndBets : Model -> Html Msg
viewTurnScoreAndBets model =
    div
        [ css
            [ borderRadius (px 25)
            , maxWidth maxContent
            , margin auto
            , overflow hidden
            , padding (px 20)
            , property "display" "grid"
            , property "grid-template-rows" "[cv] max-content [slider] max-content [bets] 50px"
            , property "grid-template-columns" "max-content max-content max-content"
            -- , property "row-gap" " 30px"
            , neumorphicShadow 5 10 model.theme.colors.lightShadow False
            , transition
                [ Css.Transitions.boxShadow3 (if model.showSettings then 0 else 150) 0 (Css.Transitions.cubicBezier 1.0 0.0 1 0.0)
                ]
            , batch
                ( case model.scorer.firstOut of
                    Team _ ->
                        [neumorphicShadow 5 15 model.theme.colors.ctaShadow False]
                    _ ->
                        []
                )
            ]
        ]
        [ viewConsecutiveVictories model
        , viewTurnScores model
        , viewBetRow model
        ]


viewBetRow : Model -> Html Msg
viewBetRow model =
    div
        [ css
            [ displayFlex
            , property "justify-content" "space-evenly"
            , property "grid-row" "bets"
            , property "grid-column" "1 / end"
            ]
        ]
        (case model.settingBet of
            Person player ->
                [ shield (ChangeSettingBet NoOne) True
                , viewBetChoice model player
                ]

            NoOne ->
                [ viewBets model model.scorer.north model.scorer.south
                , viewBets model model.scorer.west model.scorer.east
                ]
        )


viewBetChoice : Model -> Player -> Html Msg
viewBetChoice model player =
    div
        [ css
            [ border3 (px 2) solid (hex model.theme.colors.border)
            , borderRadius (px 20)
            , padding (px 10)
            , displayFlex
            , alignItems center
            , margin auto
            , boxSizing borderBox
            , backgroundColor (hex model.theme.colors.menuBackground)
            , position relative
            ]
        ]
        [ div
            [ onClick (ChangePlayerBet player Tichu True)
            , css
                [ cursor pointer
                , borderRight3 (px 2) solid (hex model.theme.colors.border)
                , display inlineBlock
                , marginRight (px 10)
                , padding2 (px 5) (px 10)
                , border3 (px 2) solid (hex model.theme.colors.border)
                , borderRadius (px 10)
                , boxSizing borderBox
                , textAlign center
                , cursor pointer
                , backgroundColor (hex model.theme.colors.background)
                ]
            ]
            [ text "Tichu" ]
        , div
            [ onClick (ChangePlayerBet player GrandTichu True)
            , css
                [ cursor pointer
                , padding2 (px 5) (px 10)
                , border3 (px 2) solid (hex model.theme.colors.border)
                , borderRadius (px 10)
                , boxSizing borderBox
                , textAlign center
                , cursor pointer
                , backgroundColor (hex model.theme.colors.background)
                ]
            ]
            [ text "Grand Tichu" ]
        ]


viewBets : Model -> ( Player, Bet ) -> ( Player, Bet ) -> Html Msg
viewBets model ( player1, bet1 ) ( player2, bet2 ) =
    div
        [ css
            [ borderRadius (px 10)
            -- , padding (px 10)
            , displayFlex
            , flexDirection column
            , alignItems center
            , boxSizing borderBox
            , backgroundColor (hex model.theme.colors.background)
            , maxHeight maxContent
            , position relative
            ]
        ]
        [ viewAddBet model ( player1, bet1 ) (bet2 == Zero)
        -- , if bet1 /= Zero then
        --     if bet2 == Zero then
        --         viewAddBetMin model ( player2, bet2 )

        --     else
        --         viewAddBet model ( player2, bet2 ) True

        --   else
        --     text ""
        ]


viewAddBet : Model -> ( Player, Bet ) -> Bool -> Html Msg
viewAddBet model ( player, bet ) showClose =
    let
        shadow = case model.scorer.firstOut of
                Team _ -> model.theme.colors.darkShadow
                _ -> model.theme.colors.lightShadow
    in
    div
        [ css
            [ position relative
            , nthChild "2" [ marginTop (px 10) ]
            ]
        ]
        [ if bet == Zero then
            button
                [ css
                    [ padding2 (px 15) (px 20)
                    -- , border3 (px 2) solid model.theme.colors.border
                    , neumorphicShadow 5 10 shadow False
                    , borderRadius (px 10)
                    -- , width (px 86)
                    , boxSizing borderBox
                    , textAlign center
                    , cursor pointer
                    , fontStyle italic
                    , backgroundColor (hex model.theme.colors.background)
                    , active
                        [ neumorphicShadow 5 10 shadow True
                        ]
                    ]
                , onClick (ChangeSettingBet (Person player))
                ]
                [ text "add bet" ]

          else
            viewBet model ( player, bet ) showClose
        ]


viewAddBetMin : Model -> ( Player, Bet ) -> Html Msg
viewAddBetMin model ( player, _ ) =
    div
        [ css
            [ position absolute
            , backgroundColor (hex model.theme.colors.menuBackground)
            , left (pct 50)
            , transform (translate2 (pct -50) (pct -50))
            , top (pct 100)
            , padding2 zero (px 10)
            , height (px 20)
            , borderBottom3 (px 2) solid (hex model.theme.colors.border)
            , borderBottomLeftRadius (px 10)
            , borderBottomRightRadius (px 10)
            , borderTop zero
            , boxSizing borderBox
            , textAlign center
            , lineHeight initial
            , cursor pointer
            ]
        , onClick (ChangeSettingBet (Person player))
        ]
        [ text "+" ]


viewBet : Model -> ( Player, Bet ) -> Bool -> Html Msg
viewBet model ( player, bet ) showClose =
    let
        betLabel =
            if bet == Tichu then
                "Tichu"

            else
                "Grand"

        playerId =
            getPlayerId player

        successful =
            bet
                /= Zero
                && (case model.scorer.firstOut of
                        One p ->
                            p == player

                        Team ( _, Just p ) ->
                            p == player

                        _ ->
                            False
                   )
    in
    div
        [ css
            [ position relative
            , borderRadius (px 10)
            , padding (px 10)
            , batch
                ( if successful then
                    [ backgroundColor (hex "71be44")
                    , neumorphicShadow 3 10 { light = "4f862e5c", dark = "71be4457"} False
                    ]
                 else
                    [ backgroundColor (hex model.theme.colors.background)
                    , neumorphicShadow 5 10 model.theme.colors.lightShadow False
                    ]
                )
            ]
        ]
        [ div
            [ css
                [ width (px 17)
                , height (px 15)
                , cursor pointer
                , borderTopLeftRadius (px 10)
                , borderBottomRightRadius (px 5)
                -- , neumorphicShadow 5 10 model.theme.colors.lightShadow False
                , backgroundColor (hex model.theme.colors.menuBackground)
                -- , border3 (px 1) solid (hex model.theme.colors.border)
                , boxSizing borderBox
                , position absolute
                , top zero
                , left zero
                , batch
                    (if showClose then
                        []

                     else
                        [ display none ]
                    )
                ]
            , onClick (ChangePlayerBet player bet False)
            ]
            [ div
                [ css
                    [ width (px 7)
                    , lineHeight zero
                    , position absolute
                    , top (pct 50)
                    , left (pct 50)
                    , transform (translate2 (pct -50) (pct -50))
                    ]
                ]
                [ xSvg ]
            ]
        , div
            [ css
                [ displayFlex
                ]
            ]
            [ input
                [ type_ "checkbox"
                , id ("success" ++ "-" ++ playerId)
                , Html.Styled.Attributes.checked successful
                , onCheck (ChangeFirstOut player)
                , css
                    [ display none
                    ]
                ]
                []
            , label
                [ for ("success" ++ "-" ++ playerId)
                , css
                    [ padding (px 5)
                    , cursor pointer
                    -- , backgroundColor (hex model.theme.colors.background)
                    -- , borderTopLeftRadius (px 8)
                    -- , borderBottomLeftRadius (px 8)
                    , width (px 54)
                    , boxSizing borderBox
                    , textAlign center
                    ]
                ]
                [ text betLabel ]
            , label
                [ css
                    [ padding (px 5)
                    , cursor pointer
                    , displayFlex
                    , alignItems center
                    , justifyContent center
                    ]
                , for ("success" ++ "-" ++ playerId)
                ]
                [ div
                    [ css
                        [ borderRadius (px 5)
                        , boxSizing borderBox
                        , width (px 18)
                        , height (px 18)
                        , fontSize (px 12)
                        , displayFlex
                        , alignItems center
                        , justifyContent center
                        , backgroundColor (hex "FFF")
                        , color (hex "111")
                        , fontWeight bold
                        , neumorphicShadow
                            5
                            10
                            ( if successful then
                                model.theme.colors.darkShadow
                              else
                                model.theme.colors.lightShadow
                            )
                            False
                        ]
                    ]
                    [ text
                        (if successful then
                            "✓"

                         else
                            " "
                        )
                    ]
                ]
            ]
        ]


viewTurnScores : Model -> Html Msg
viewTurnScores model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , property "grid-row-start" "cv"
            , property "grid-row-end" "slider"
            , property "grid-column" "2"
            , padding2 (px 30) zero
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
            , position relative
            , bottom zero
            , transition [ Css.Transitions.bottom3 200 0 easeInOut ]
            , batch
                (case model.scorer.firstOut of
                    Team _ ->
                        [ bottom (px -25)
                        ]

                    _ ->
                        []
                )
            ]
        ]
        [ viewTeamTurnScore model model.scorer.vertTurnScore Vertical
        , viewTeamTurnScore model (100 - model.scorer.vertTurnScore) Horizontal
        ]


viewTeamTurnScore : Model -> Int -> Team -> Html Msg
viewTeamTurnScore model teamScore team =
    let
        widthPx =
            60
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
                --, backgroundColor (hex model.theme.colors.menuBackground)
                , border3 (px 1) solid (hex model.theme.colors.border)
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


viewConsecutiveVictories : Model -> Html Msg
viewConsecutiveVictories model =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , property "grid-row" "cv"
            , property "grid-column" "1 / end"
            ]
        ]
        [ viewConsecutiveVictoryButton
            model
            "vert-cv"
            Vertical
            (case model.scorer.firstOut of
                Team ( Vertical, _ ) ->
                    True

                _ ->
                    False
            )
        , viewConsecutiveVictoryButton
            model
            "horz-cv"
            Horizontal
            (case model.scorer.firstOut of
                Team ( Horizontal, _ ) ->
                    True

                _ ->
                    False
            )
        ]


viewConsecutiveVictoryButton : Model -> String -> Team -> Bool -> Html Msg
viewConsecutiveVictoryButton model elemid team ischecked =
    div
        [ css
            [ width (px 45)
            , height (px 45)
            , batch
                ( case model.scorer.firstOut of
                    Team _ -> if ischecked then [ position relative ] else []
                    _ -> [ position relative ]
                )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , id elemid
            , Html.Styled.Attributes.checked ischecked
            , onCheck (ConsecutiveVictory team)
            , css
                [ display none
                ]
            ]
            []
        , div
            [ css
                [ borderRadius (pct 50)
                , position absolute
                , top zero
                , backgroundColor (hex model.theme.colors.cta)
                , left (pct 50)
                , top (pct 50)
                , transform (translate2 (pct -50) (pct -50))
                , width zero
                , height zero
                , batch
                    ( if ischecked then
                        [ width (vw 200)
                        , height (vw 200)
                        , transition
                            [ Css.Transitions.width3 300 0 (Css.Transitions.cubicBezier 1.0 0.05 0.6 1)
                            , Css.Transitions.height3 300 0 (Css.Transitions.cubicBezier 1.0 0.05 0.6 1)
                            ]
                        ]
                        else
                        [ transition
                            [ Css.Transitions.width3 300 0 Css.Transitions.easeOut
                            , Css.Transitions.height3 300 0 Css.Transitions.easeOut
                            ]
                        ]
                    )
                ]
            ]
            [
            ]
        , label
            [ for elemid
            , css
                [ border3 (px 0) solid (hex model.theme.colors.border)
                , display inlineFlex
                , alignItems center
                , boxSizing borderBox
                , batch
                    ( case model.scorer.firstOut of
                        Team _ -> if ischecked then [ position relative ] else []
                        _ -> [ position relative ]
                    )
                --, backgroundColor (hex model.theme.colors.border)
                , cursor pointer
                , padding (px 10)
                , borderRadius (pct 50)
                , neumorphicShadow 5 10 model.theme.colors.lightShadow False
                , transition
                    [ Css.Transitions.backgroundColor2 200 0
                    , Css.Transitions.borderColor2 200 0
                    ]
                , batch
                    (if ischecked then
                        [ backgroundColor (hex model.theme.colors.cta)
                        , borderColor (hex model.theme.colors.cta)
                        , neumorphicShadow 5 10 model.theme.colors.darkShadow True
                        ]
                     else
                        [ 
                        ]
                    )
                , case team of
                    -- Left
                    Vertical ->
                        batch
                            [-- borderTopLeftRadius (px 25)
                            --, borderBottomLeftRadius (px 25)
                            -- , paddingLeft (px 16)
                            -- , textAlign right
                            ]

                    -- Right
                    Horizontal ->
                        batch
                            [ --borderTopRightRadius (px 25)
                            --, borderBottomRightRadius (px 25)
                            --, textAlign left
                            ]
                ]
            ]
            [ div
                [ css
                    [ width (px 25)
                    , height (px 25)
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
            , alignItems center
            , position relative
            , margin auto
            , padding2 zero (px 20)
            ]
        ]
        [ div
            [ css
                [ overflow hidden
                , width (px 240)
                , position relative
                ]
            ]
            [ viewSlider model
            , viewConsecutiveVictoryOverlay model
                ( case model.scorer.firstOut of
                    Team _ -> True
                    _ -> False
                )
            ]
        ]


viewConsecutiveVictoryOverlay : Model -> Bool -> Html Msg
viewConsecutiveVictoryOverlay model active =
    button
        [ css
            [ position absolute
            , left (pct 50)
            , top (px 20)
            , property "transform-origin" "center"
            , transforms
                [ translate2 (pct -50) (pct -50)
                , scale 0
                ]
            -- , width (px 240)
            , overflow hidden
            -- , lineHeight (px 30)
            , width (px 230)
            , height (px 30)
            , backgroundColor (hex model.theme.colors.cta)
            , color (hex model.theme.colors.ctaText)
            , borderRadius (px 20)
            , textAlign center
            , batch
                (if active then
                    [ transforms
                        [ translate2 (pct -50) (pct -50)
                        , scale 1
                        ]
                    , transition
                        [ Css.Transitions.transform3 250 0 (Css.Transitions.cubicBezier 0.61 0.59 0.49 1.15)
                        ]
                    ]

                 else
                    [ transition
                        [ Css.Transitions.transform3 250 0 (Css.Transitions.cubicBezier 0.46 -0.2 0.73 0.77)
                        ]
                    ]
                )
            ]
        , onClick (ConsecutiveVictory Vertical False)
        ]
        [ text "Consecutive victory" ]


viewSlider : Model -> Html Msg
viewSlider model =
    range
        { min = -25
        , max = 125
        , step = 5
        , value = model.scorer.vertTurnScore
        , onInput = ChangeTeamScore
        }
        { height = 40
        , padding = 5
        , background = hex model.theme.colors.background
        , trackBorderRadius = 20
        , thumbHeight = 30
        , thumbWidth = 40
        , thumbColor = hex model.theme.colors.pop
        , thumbBorderRadius = 20
        , additional =
            [ border3 (px 1) solid (hex model.theme.colors.border)
            --, borderBottom3 (px 0) solid (hex model.theme.colors.border)
            , neumorphicShadow 5 10 model.theme.colors.lightShadow True
            , batch
                ( case model.scorer.firstOut of
                    Team _ -> [ pointerEvents none ]
                    _ -> []
                )
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
                , backgroundColor (hex model.theme.colors.cta)
                , color (hex model.theme.colors.ctaText)
                , borderRadius (px 10)
                , marginLeft auto
                , neumorphicShadow 3 15 model.theme.colors.ctaShadow False
                , active
                    [ neumorphicShadow 3 15 model.theme.colors.ctaShadow True
                    , color (hex model.theme.colors.ctaText)
                    ]
                ]
            , onClick Score
            ]
            [ text "Score" ]
        , iconButton
            model
            undoSvg
            [ marginLeft (px 15)
            , marginRight auto
            ]
            "Undo"
            Undo
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    div
        [ css
            [ borderRadius (px 20)
            , padding (px 10)
            , backgroundColor (hex model.theme.colors.menuBackground)
            , boxShadow4 (px 10) (px 10) (px 10) (hex model.theme.colors.lightShadow.dark)
            , width
                (if model.showAbout then
                    px 300

                 else
                    px 175
                )
            , transition [ Css.Transitions.width3 100 0 easeInOut ]
            , textAlign left
            , marginBottom (px 10)
            ]
        ]
        (if model.showAbout then
            [ viewAbout model ]

         else
            defaultSettings model
        )


defaultSettings : Model -> List (Html Msg)
defaultSettings model =
    let
        buttonStyles =
            [ cursor pointer
            , width (pct 100)
            , padding2 (px 6) zero
            , backgroundColor (hex model.theme.colors.background)
            , neumorphicShadow 5 10 model.theme.colors.darkShadow False
            , boxSizing borderBox
            , color inherit
            , borderRadius (px 10)
            ]
    in
    [ button
        [ css
            [ batch buttonStyles
            , textAlign left
            , padding (px 12)
            ]
        ]
        (themeSettings model)
    , hr 2 (hex model.theme.colors.border) [ margin2 (px 10) zero ]
    , div
        [ css
            [ displayFlex
            , property "justify-content" "space-evenly"
            ]
        ]
        [ iconButton
            model
            trashSvg
            [ backgroundColor (hex model.theme.colors.background) ]
            "Reset"
            (ShowConfirmation "Are you sure you want to reset?" Clear)
        , iconButton
            model
            (div [ css [ textAlign center, fontSize (px 18), fontWeight bold ] ] [ text "i" ])
            [ backgroundColor (hex model.theme.colors.background) ]
            "About"
            (ShowAbout True)
        , iconButton
            model
            undoSvg
            [ backgroundColor (hex
                (if model.updateAvailable then
                    model.theme.colors.cta

                 else
                    model.theme.colors.background
                ))
            , color (hex
                (if model.updateAvailable then
                    model.theme.colors.ctaText

                 else
                    model.theme.colors.text
                ))
            , transform (scale2 -1 1)
            ]
            "Reload"
            Update
        ]
    ]


themeSettings : Model -> List (Html Msg)
themeSettings model =
    [ div
        [ css
            [ paddingBottom (px 5)
            , marginBottom (px 5)
            , textAlign center
            , fontWeight bold
            , borderBottom3 (px 2) solid (hex model.theme.colors.border)
            ]
        ]
        [ text "Theme" ]
    , div
        []
        (List.map
            (\theme ->
                li
                    [ onClick (ChangeLighting theme.id)
                    , type_
                        (if model.theme.id == theme.id then
                            "filled"

                         else
                            "circle"
                        )
                    , css
                        [ batch
                            (if model.theme.id == theme.id then
                                [ fontWeight bold ]

                             else
                                []
                            )
                        , marginBottom (px 5)
                        , cursor pointer
                        , lastChild [ marginBottom zero ]
                        ]
                    ]
                    [ text theme.name
                    ]
            )
            (Dict.values themes)
        )
    ]


viewAbout : Model -> Html Msg
viewAbout model =
    div
        [ css
            [ color inherit
            , textAlign left
            ]
        ]
        [ div
            [ css
                [ paddingBottom (px 5)
                , marginBottom (px 5)
                , textAlign center
                , fontWeight bold
                , borderBottom3 (px 2) solid (hex model.theme.colors.border)
                ]
            ]
            [ text "About" ]
        , div
            []
            [ aboutEntry "Version" model.currentVersion.version
            , aboutEntry "Developed by" "Justin Renjilian"
            , hr 2 (hex model.theme.colors.border) [ margin2 (px 10) zero ]
            , a
                [ href "https://github.com/freshvictory/tichu-web/issues/new"
                , Html.Styled.Attributes.target "_blank"
                , title "hep"
                , css
                    [ color inherit
                    , visited [ color inherit ]
                    , fontWeight bold
                    , textDecoration none
                    , fontStyle italic
                    , display block
                    , boxSizing borderBox
                    , margin2 (px 5) zero
                    ]
                ]
                [ text "I'm having a problem..." ]
            ]
        ]


aboutEntry : String -> String -> Html Msg
aboutEntry label value =
    li
        [ css
            [ marginBottom (px 5)
            , lastChild [ marginBottom zero ]
            ]
        ]
        [ text (label ++ " ")
        , span
            [ css
                [ fontWeight bold
                ]
            ]
            [ text value ]
        ]


settingsGear : Model -> Html Msg
settingsGear model =
    div
        [ css
            [ height (px 30)
            , position absolute
            , bottom zero
            , right zero
            , padding inherit
            , margin (px 20)
            , textAlign right
            ]
        ]
        [ input
            [ type_ "checkbox"
            , id "settings-toggle"
            , Html.Styled.Attributes.checked model.showSettings
            , onCheck ToggleSettings
            , css
                [ display none
                ]
            ]
            []
        , label
            [ for "settings-toggle"
            , css
                [ position absolute
                , right zero
                , width (px 30)
                , height (px 30)
                , padding (px 5)
                , borderRadius (pct 50)
                , color (hex model.theme.colors.border)
                , cursor pointer
                , neumorphicShadow 5 10 model.theme.colors.lightShadow False
                , batch
                    (if model.updateAvailable then
                        [ after
                            [ property "content" "''"
                            , width (px 8)
                            , height (px 8)
                            , borderRadius (px 8)
                            , backgroundColor (hex model.theme.colors.pop)
                            , position absolute
                            , top (px -4)
                            , right (px -4)
                            ]
                        ]

                     else
                        []
                    )
                , batch
                    (if model.showSettings then
                        [ neumorphicShadow 5 10 model.theme.colors.lightShadow True ]
                    else []
                    )
                ]
            ]
            [ gearSvg ]
        ]


iconButton : Model -> Html Msg -> List Style -> String -> Msg -> Html Msg
iconButton model icon styles t onclick =
    button
        [ css
            [ width (px 34)
            , height (px 34)
            , display inlineBlock
            , cursor pointer
            , padding (px 7)
            --, border3 (px 2) solid (hex model.theme.colors.border)
            , borderRadius (pct 50)
            , backgroundColor (hex model.theme.colors.menuBackground)
            , neumorphicShadow 2 10 model.theme.colors.lightShadow False
            , active
                [ neumorphicShadow 2 10 model.theme.colors.lightShadow True
                ]
            , batch styles
            ]
        , title t
        , onClick onclick
        ]
        [ icon ]


confirm : Model -> Html Msg
confirm model =
    case model.confirm of
        Hidden ->
            text ""

        Active query msg ->
            div
                [ css
                    [ position absolute
                    , left zero
                    , right zero
                    , top zero
                    , bottom zero
                    ]
                ]
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
                    [ div
                        [ css
                            [ backgroundColor (hex model.theme.colors.background)
                            , borderRadius (px 30)
                            -- , border3 (px 3) solid (hex model.theme.colors.border)
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
                                , css
                                    [ confirmButtonStyle
                                    , backgroundColor (hex model.theme.colors.background)
                                    , color (hex model.theme.colors.text)
                                    , neumorphicShadow 5 15 model.theme.colors.lightShadow False
                                    , active
                                        [ neumorphicShadow 5 15 model.theme.colors.lightShadow True
                                        ]
                                    ]
                                ]
                                [ text "No" ]
                            , button
                                [ onClick msg
                                , css
                                    [ confirmButtonStyle
                                    , backgroundColor (hex model.theme.colors.cta)
                                    , color (hex model.theme.colors.ctaText)
                                    , neumorphicShadow 3 15 model.theme.colors.ctaShadow False
                                    , marginLeft auto
                                    , active
                                        [ neumorphicShadow 3 15 model.theme.colors.ctaShadow True
                                        ]
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

  - <https://stackoverflow.com/a/53017452>

-}
decodeAsTuple2 : String -> Decoder a -> String -> Decoder b -> Decoder ( a, b )
decodeAsTuple2 fieldA decoderA fieldB decoderB =
    let
        result : a -> b -> ( a, b )
        result valueA valueB =
            ( valueA, valueB )
    in
    succeed result
        |> andMap (field fieldA decoderA)
        |> andMap (field fieldB decoderB)



-- HTTP


checkForUpdate : Cmd Msg
checkForUpdate =
    Http.get
        { url = "https://beta--tichu.netlify.com/version.json"
        , expect = Http.expectJson CheckedVersion versionDecoder
        }
