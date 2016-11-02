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
        , minesAround
        , hasMine
        , isCovered
        , isMarked
        , isSolved
        , isFailed
        , toString
        )


type Square
    = Square MinePresence CoverPresence MinesAround


type alias MinesAround =
    Int


type MinePresence
    = Mine
    | Empty


type CoverPresence
    = Covered
    | Uncovered
    | Marked


empty : Square
empty =
    Square Empty Covered 0


mined : Square
mined =
    Square Mine Covered 0


setMinesAround : Square -> Int -> Square
setMinesAround (Square m c _) mines =
    Square m c mines


minesAround : Square -> Int
minesAround (Square _ _ count) =
    count


hasMine : Square -> Bool
hasMine square =
    case square of
        Square Mine _ _ ->
            True

        _ ->
            False


isCovered : Square -> Bool
isCovered =
    not << isUncovered


isUncovered : Square -> Bool
isUncovered (Square _ cover _) =
    cover == Uncovered


isMarked : Square -> Bool
isMarked (Square _ marked _) =
    marked == Marked


uncover : Square -> Square
uncover square =
    case square of
        Square m Covered c ->
            Square m Uncovered c

        _ ->
            square


isSolved : Square -> Bool
isSolved square =
    case square of
        Square Empty Uncovered _ ->
            True

        Square Mine Covered _ ->
            True

        Square Mine Marked _ ->
            True

        _ ->
            False


isFailed : Square -> Bool
isFailed square =
    hasMine square && not (isCovered square)


toggleMark : Square -> Square
toggleMark square =
    case square of
        Square m Covered c ->
            Square m Marked c

        Square m Marked c ->
            Square m Covered c

        _ ->
            square


toString : Square -> String
toString square =
    case square of
        Square _ Marked _ ->
            "🚩"

        Square _ Covered _ ->
            "◻️"

        Square Mine Uncovered _ ->
            "💣"

        Square Empty Uncovered 0 ->
            ""

        Square Empty Uncovered count ->
            Basics.toString count
