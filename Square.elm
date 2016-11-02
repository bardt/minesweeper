module Square
    exposing
        ( Square
          -- Constructors
        , empty
        , mined
          -- Setters
        , setMinesAround
        , uncover
        , toggleMark
          -- Getters
        , location
        , minesAround
        , hasMine
        , isCovered
        , isMarked
        , isSolved
        , isFailed
        , toString
        )

import Matrix exposing (Location)


type Square
    = Square Location MinePresence CoverPresence MinesAround


type alias MinesAround =
    Int


type MinePresence
    = Mine
    | Empty


type CoverPresence
    = Covered
    | Uncovered
    | Marked


empty : Location -> Square
empty loc =
    Square loc Empty Covered 0


mined : Location -> Square
mined loc =
    Square loc Mine Covered 0


setMinesAround : Square -> Int -> Square
setMinesAround (Square l m c _) mines =
    Square l m c mines


minesAround : Square -> Int
minesAround (Square _ _ _ count) =
    count


location : Square -> Location
location (Square loc _ _ _) =
    loc


hasMine : Square -> Bool
hasMine square =
    case square of
        Square _ Mine _ _ ->
            True

        _ ->
            False


isCovered : Square -> Bool
isCovered =
    not << isUncovered


isUncovered : Square -> Bool
isUncovered (Square _ _ cover _) =
    cover == Uncovered


isMarked : Square -> Bool
isMarked (Square _ _ marked _) =
    marked == Marked


uncover : Square -> Square
uncover square =
    case square of
        Square l m Covered c ->
            Square l m Uncovered c

        _ ->
            square


isSolved : Square -> Bool
isSolved square =
    case square of
        Square _ Empty Uncovered _ ->
            True

        Square _ Mine Covered _ ->
            True

        Square _ Mine Marked _ ->
            True

        _ ->
            False


isFailed : Square -> Bool
isFailed square =
    hasMine square && not (isCovered square)


toggleMark : Square -> Square
toggleMark square =
    case square of
        Square l m Covered c ->
            Square l m Marked c

        Square l m Marked c ->
            Square l m Covered c

        _ ->
            square


toString : Square -> String
toString square =
    case square of
        Square _ _ Marked _ ->
            "🚩"

        Square _ _ Covered _ ->
            "◻️"

        Square _ Mine Uncovered _ ->
            "💣"

        Square _ Empty Uncovered 0 ->
            ""

        Square _ Empty Uncovered count ->
            Basics.toString count
