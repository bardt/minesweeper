module App exposing (..)

import Types exposing (..)
import Html exposing (Html, text, div, h1, table, tr, td, button, fieldset, label, input)
import Html.Events exposing (onClick, onWithOptions)
import Game exposing (Game, difficultyLevels)


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { screen : Screen
    , game : Game
    }


initialModel : Model
initialModel =
    { game = Game.initialGame
    , screen = Start
    }



-- UPDATE


type Msg
    = ChangeScreen Screen
    | StartNewGame DifficultyLevel
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewGame level ->
            let
                ( newGame, gameCmd ) =
                    Game.initWithLevel level
            in
                ( { model
                    | screen = Types.Game
                    , game = newGame
                  }
                , Cmd.map GameMsg gameCmd
                )

        ChangeScreen screen ->
            ( { model | screen = screen }, Cmd.none )

        GameMsg gameMsg ->
            let
                ( updatedGameModel, gameCmd ) =
                    Game.update gameMsg model.game
            in
                ( { model
                    | game = updatedGameModel
                  }
                , Cmd.map GameMsg gameCmd
                )



-- VIEW


view : Model -> Html Msg
view model =
    case model.screen of
        Start ->
            startScreen

        Difficulty ->
            difficultyScreen

        Game ->
            gameScreen model


startScreen : Html Msg
startScreen =
    div []
        [ h1 []
            [ text "Minesweeper" ]
        , button [ onClick <| ChangeScreen Difficulty ]
            [ text "New game" ]
        ]


difficultyScreen : Html Msg
difficultyScreen =
    div []
        [ h1 []
            [ text "Minesweeper" ]
        , div []
            [ difficultyButton "Beginner" difficultyLevels.beginner
            , difficultyButton "Intermediate" difficultyLevels.intermediate
            , difficultyButton "Expert" difficultyLevels.expert
            ]
        ]


difficultyButton : String -> DifficultyLevel -> Html Msg
difficultyButton caption level =
    button
        [ onClick <| StartNewGame level
        ]
        [ text caption ]


gameScreen : Model -> Html Msg
gameScreen model =
    Html.map GameMsg (Game.view model.game)
