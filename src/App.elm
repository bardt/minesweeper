module App exposing (..)

import Square exposing (Square)
import Types exposing (..)
import Html exposing (Html, text, div, h1, table, tr, td, button, fieldset, label, input)
import Html.Events exposing (onClick, onWithOptions)
import Html.Attributes exposing (style, type_, name, checked)
import Matrix exposing (Matrix, Location)
import Json.Decode as Json
import Random exposing (Generator)
import Map
import Rest exposing (..)
import Gestures


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Random.generate NewMap (Map.random initialModel.difficultyLevel) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


initialModel : Model
initialModel =
    { map = Map.empty
    , screen = Start
    , gameStatus = Started
    , difficultyLevel = difficultyLevels.beginner
    }


difficultyLevels : DifficultyLevels
difficultyLevels =
    { beginner = DifficultyLevel 9 9 10
    , intermediate = DifficultyLevel 16 16 40
    , expert = DifficultyLevel 16 30 99
    }


countMinesTotal : Map -> Int
countMinesTotal m =
    Matrix.flatten m
        |> List.map (boolToInt << Square.hasMine)
        |> List.sum


countUsedMarks : Map -> Int
countUsedMarks m =
    Matrix.flatten m
        |> List.filter Square.isMarked
        |> List.length


checkGameStatus : Map -> GameStatus
checkGameStatus map =
    if Map.isSolved map then
        Won
    else if Map.isFailed map then
        Failed
    else
        Started



-- UPDATE


type Msg
    = ChangeScreen Screen
    | StartNewGame DifficultyLevel
    | Uncover Location
    | Mark Location
    | NewMap Map


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        uncoveredMap location =
            if model.gameStatus == Started then
                Map.uncover model.map location
            else
                model.map

        markedMap location =
            if model.gameStatus == Started then
                Map.mark model.map location
            else
                model.map
    in
        case msg of
            Uncover location ->
                ( { model
                    | map =
                        uncoveredMap location
                    , gameStatus =
                        checkGameStatus (uncoveredMap location)
                  }
                , Cmd.none
                )

            Mark location ->
                ( { model
                    | map = markedMap location
                  }
                , Cmd.none
                )

            StartNewGame level ->
                ( { model
                    | difficultyLevel = level
                    , screen = Game
                  }
                , Random.generate NewMap (Map.random level)
                )

            NewMap map ->
                ( { model
                    | map = map
                    , gameStatus = Started
                  }
                , Cmd.none
                )

            ChangeScreen screen ->
                ( { model | screen = screen }, Cmd.none )



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


gameScreen : Model -> Html Msg
gameScreen model =
    let
        minesTotal =
            countMinesTotal model.map

        marksLeft =
            minesTotal - countUsedMarks model.map
    in
        div []
            [ button [ onClick <| StartNewGame model.difficultyLevel ]
                [ text "New game" ]
            , div []
                [ div [] [ text <| "💣" ++ toString minesTotal ]
                , div [] [ text <| "🚩" ++ toString marksLeft ]
                , mapView model.map
                ]
            ]


difficultyButton : String -> DifficultyLevel -> Html Msg
difficultyButton caption level =
    button
        [ onClick <| StartNewGame level
        ]
        [ text caption ]


mapView : Map -> Html Msg
mapView map =
    let
        htmlMap =
            Matrix.map squareView map

        tableRows =
            List.map mapRowView (Matrix.toList htmlMap)
    in
        table [] tableRows


mapRowView : List (Html Msg) -> Html Msg
mapRowView rowHtml =
    tr [] rowHtml


squareView : Square -> Html Msg
squareView square =
    td
        [ style [ ( "width", "2em" ), ( "height", "2em" ), ( "-webkit-user-select", "none" ) ]
        , onClick <| Uncover <| Square.location square
        , onRightClick <| Mark <| Square.location square
        , Gestures.onTap <| Uncover <| Square.location square
        , Gestures.onLongTap <| Mark <| Square.location square
        ]
        [ text <| Square.toString square
        ]


onRightClick : Msg -> Html.Attribute Msg
onRightClick msg =
    let
        preventOpt =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "contextmenu" preventOpt (Json.succeed msg)
