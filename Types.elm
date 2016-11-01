module Types exposing (..)

import Matrix exposing (Matrix, Location)


type alias Model =
    { map : Map
    , gameStatus : GameStatus
    , difficultyLevel : DifficultyLevel
    }


type alias Map =
    Matrix Square


type alias Square =
    ( MinePresence, CoverPresence, MinesAround )


type alias MinesAround =
    Int


type MinePresence
    = Mine
    | Empty


type CoverPresence
    = Covered
    | Uncovered
    | Marked


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
