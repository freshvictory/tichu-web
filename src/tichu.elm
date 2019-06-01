import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


-- MAIN

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Tichu", body = [ model |> view |> toUnstyled ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- MODEL

type alias Model =
  { teams : Dict Int Team
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { teams =
        Dict.fromList
          [ ( 1, { name = "Team 1", score = 0 } )
          , ( 2, { name = "Team 2", score = 0 } )
          ]
    }
  , Cmd.none
  )


type alias Team =
  { name : String
  , score : Int
  }


getTeamName : Int -> Dict Int Team -> String
getTeamName teamNumber dict =
  case ( Dict.get teamNumber dict ) of
    Nothing -> "Team " ++ String.fromInt teamNumber
    Just team -> team.name


getTeamScore : Int -> Dict Int Team -> Int
getTeamScore teamNumber dict =
  case ( Dict.get teamNumber dict ) of
    Nothing -> 0
    Just team -> team.score



-- UPDATE

type Msg
  = ChangeName Int String
  | ChangeScore Int Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName teamNumber name ->
      ( { model | teams = 
            Dict.insert teamNumber (updateName model teamNumber name) model.teams }
      , Cmd.none )
    ChangeScore teamNumber score ->
      ( { model | teams =
          Dict.insert teamNumber (updateScore model teamNumber score) model.teams }
      , Cmd.none
      )


updateName : Model -> Int -> String -> Team
updateName model teamNumber name =
  case Dict.get teamNumber model.teams of
    Nothing -> { name = name, score = 0 }
    Just team -> { team | name = name }


updateScore : Model -> Int -> Int -> Team
updateScore model teamNumber score =
  case Dict.get teamNumber model.teams of
    Nothing -> { name = getTeamName teamNumber model.teams, score = score }
    Just team -> { team | score = team.score + score }


-- VIEW

view : Model -> Html Msg
view model =
  div [ css [ displayFlex, flexDirection column ] ]
    [ div [css [displayFlex, flexDirection row ] ]
      [ viewTeamName model 1
      , viewTeamName model 2
      ]
    , div [ css [ displayFlex, flexDirection row ] ]
      [ viewTeamScore model 1
      , viewTeamScore model 2
      ]
    , button [ onClick (ChangeScore 1 20) ] [ text "score" ]
    ]
  

viewTeamName : Model -> Int -> Html Msg
viewTeamName model teamNumber =
  input [ placeholder (getTeamName teamNumber model.teams)
        , onInput (ChangeName teamNumber)
        ]
        []
  

viewTeamScore : Model -> Int -> Html Msg
viewTeamScore model teamNumber =
  text
    ( (getTeamName teamNumber model.teams)
    ++ ": "
    ++ (String.fromInt (getTeamScore teamNumber model.teams))
    )
