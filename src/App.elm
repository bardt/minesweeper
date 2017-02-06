module App exposing (..)

import Game exposing (Game, difficultyLevels)
import Html exposing (Html, button, div, fieldset, h1, h2, input, label, table, td, text, tr)
import Html.Events exposing (onClick, onWithOptions)
import Types exposing (..)


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
    = GoToStartScreen
    | GoToDifficultyScreen
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

        GoToStartScreen ->
            ( { model | screen = Start }, Cmd.none )

        GoToDifficultyScreen ->
            ( { model | screen = Difficulty }, Cmd.none )

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
            [ text "🚩👷💣" ]
        , button [ onClick <| GoToDifficultyScreen ]
            [ text "🏁" ]
        ]


difficultyScreen : Html Msg
difficultyScreen =
    div []
        [ h1 []
            [ text "🚩👷💣" ]
        , div []
            [ difficultyButton "😀" difficultyLevels.beginner
            , difficultyButton "\x1F913" difficultyLevels.intermediate
            , difficultyButton "😈" difficultyLevels.expert
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
    let
        repeat =
            button [ onClick (StartNewGame model.game.level) ] [ text "🔁" ]

        backToStart =
            button [ onClick GoToStartScreen ] [ text "🔙" ]
    in
        div []
            [ div []
                [ (case model.game.status of
                    InProgress ->
                        div []
                            [ div [] [ text <| "💣" ++ toString (Game.countMinesTotal model.game.map) ]
                            , div [] [ text <| "🚩" ++ toString (Game.countMarksLeft model.game.map) ]
                            ]

                    Won ->
                        h2 [] [ backToStart, text "😎", repeat ]

                    Failed ->
                        h2 [] [ backToStart, text "😭", repeat ]
                  )
                ]
            , Html.map GameMsg (Game.view model.game)
            ]
