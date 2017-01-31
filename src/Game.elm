module Game
    exposing
        ( init
        , initWithLevel
        , initialGame
        , update
        , view
        , difficultyLevels
        , Game
        , Msg
        )

import Gestures
import Html exposing (Html, button, div, h2, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Map
import Matrix exposing (Location, Matrix)
import Random
import Rest exposing (boolToInt)
import Square exposing (Square)
import Types exposing (..)


type alias Game =
    { status : GameStatus
    , map : Map
    , level : DifficultyLevel
    }


type Msg
    = NewMap Map
    | Uncover Location
    | Mark Location


initialGame : Game
initialGame =
    { status = InProgress
    , map = Map.empty
    , level = difficultyLevels.beginner
    }


difficultyLevels : DifficultyLevels
difficultyLevels =
    { beginner = DifficultyLevel 9 9 10
    , intermediate = DifficultyLevel 16 16 40
    , expert = DifficultyLevel 16 30 99
    }


init : ( Game, Cmd Msg )
init =
    ( initialGame, Random.generate NewMap (Map.random initialGame.level) )


initWithLevel : DifficultyLevel -> ( Game, Cmd Msg )
initWithLevel level =
    ( { initialGame | level = level }
    , Random.generate NewMap (Map.random level)
    )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    let
        uncoveredMap location =
            if game.status == InProgress then
                Map.uncover game.map location
            else
                game.map

        markedMap location =
            if game.status == InProgress then
                Map.mark game.map location
            else
                game.map
    in
        case msg of
            NewMap map ->
                ( { game
                    | map = map
                    , status = InProgress
                  }
                , Cmd.none
                )

            Uncover location ->
                ( { game
                    | map =
                        uncoveredMap location
                    , status =
                        checkGameStatus (uncoveredMap location)
                  }
                , Cmd.none
                )

            Mark location ->
                ( { game
                    | map = markedMap location
                  }
                , Cmd.none
                )


checkGameStatus : Map -> GameStatus
checkGameStatus map =
    if Map.isSolved map then
        Won
    else if Map.isFailed map then
        Failed
    else
        InProgress


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


view : Game -> Html Msg
view game =
    let
        minesTotal =
            countMinesTotal game.map

        marksLeft =
            minesTotal - countUsedMarks game.map
    in
        div []
            [ div []
                [ (case game.status of
                    InProgress ->
                        text ""

                    Won ->
                        h2 [] [ text "The winner is you" ]

                    Failed ->
                        h2 [] [ text "You failed" ]
                  )
                , div [] [ text <| "💣" ++ toString minesTotal ]
                , div [] [ text <| "🚩" ++ toString marksLeft ]
                , mapView game.map
                ]
            ]


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
        , Gestures.onRightClick <| Mark <| Square.location square
        , Gestures.onTap <| Uncover <| Square.location square
        , Gestures.onLongTap <| Mark <| Square.location square
        ]
        [ text <| Square.toString square
        ]
