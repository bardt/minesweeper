module Types exposing (..)

import Matrix exposing (Matrix, Location)
import Square exposing (Square)


type alias Map =
    Matrix Square


type Screen
    = Start
    | Difficulty
    | Game


type GameStatus
    = InProgress
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
