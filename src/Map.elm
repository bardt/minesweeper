module Map
    exposing
        ( empty
        , random
        , hasMine
        , uncover
        , mark
        , isSolved
        , isFailed
        )

import Matrix exposing (Matrix, Location)
import Types exposing (..)
import Square exposing (Square)
import Array exposing (Array)
import Random exposing (Generator)
import Random.Array as RandomArray
import Rest exposing (..)


empty : Map
empty =
    Matrix.fromList []


random : DifficultyLevel -> Generator Map
random level =
    let
        count =
            min (level.width * level.height) level.mines

        fillMatrix : Array Location -> Map
        fillMatrix locations =
            let
                locList =
                    Array.toList locations
            in
                Matrix.matrix level.height
                    level.width
                    (\loc ->
                        if List.member loc locList then
                            Square.mined loc
                        else
                            Square.empty loc
                    )
    in
        Matrix.matrix level.height level.width (\loc -> loc)
            |> Matrix.flatten
            |> Array.fromList
            |> RandomArray.shuffle
            |> Random.map (Array.slice 0 count)
            |> Random.map fillMatrix
            |> Random.map putCountersIntoSquares


hasMine : Map -> Location -> Bool
hasMine map loc =
    Matrix.get loc map
        |> Maybe.map Square.hasMine
        |> Maybe.withDefault False


mark : Map -> Location -> Map
mark map location =
    Matrix.update location Square.toggleMark map


uncover : Map -> Location -> Map
uncover map location =
    let
        canBeUncovered =
            Matrix.get location map
                |> Maybe.map Square.isCovered
                |> Maybe.withDefault False

        noMinesAround : Bool
        noMinesAround =
            Matrix.get location map
                |> Maybe.map (\s -> Square.minesAround s == 0 && not (Square.hasMine s))
                |> Maybe.withDefault False

        nextLocationsToUncover : List Location
        nextLocationsToUncover =
            if noMinesAround then
                neighbourLocations location
            else
                []

        uncoverNext : Map -> Map
        uncoverNext m =
            List.foldl (flip uncover) m nextLocationsToUncover
    in
        -- recursion exit condition
        if canBeUncovered then
            -- uncover this one
            uncoverOne map location
                -- try to uncover all neighbouts
                |>
                    uncoverNext
        else
            map


isSolved : Map -> Bool
isSolved map =
    Matrix.flatten map
        |> List.all Square.isSolved


isFailed : Map -> Bool
isFailed map =
    Matrix.flatten map
        |> List.any Square.isFailed



-- PRIVATE


neighbourLocations : Location -> List Location
neighbourLocations location =
    let
        row =
            Matrix.row location

        col =
            Matrix.col location
    in
        [ ( row - 1, col - 1 )
        , ( row - 1, col )
        , ( row - 1, col + 1 )
        , ( row, col - 1 )
        , ( row, col + 1 )
        , ( row + 1, col - 1 )
        , ( row + 1, col )
        , ( row + 1, col + 1 )
        ]


uncoverOne : Map -> Location -> Map
uncoverOne m loc =
    Matrix.update loc Square.uncover m


putCountersIntoSquares : Map -> Map
putCountersIntoSquares map =
    let
        countEach : Location -> Square -> Square
        countEach location square =
            if Square.hasMine square then
                Square.setMinesAround square 0
            else
                neighbourLocations location
                    |> List.map (boolToInt << hasMine map)
                    |> List.sum
                    |> Square.setMinesAround square
    in
        Matrix.mapWithLocation countEach map
