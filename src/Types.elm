module Types exposing (..)

import Matrix exposing (Matrix, Location)
import Square exposing (Square)


type alias Model =
    { map : Map
    , gameStatus : GameStatus
    , difficultyLevel : DifficultyLevel
    }


type alias Map =
    Matrix Square


type GameStatus
    = Started
    | Failed
    | Won


type alias DifficultyLevel =
    { height : Int
    , width : Int
    , mines : Int
    }


type alias DifficultyLevels =
    { beginner : DifficultyLevel
    , intermediate : DifficultyLevel
    , expert : DifficultyLevel
    }