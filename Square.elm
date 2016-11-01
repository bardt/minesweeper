module Square exposing (..)

import Types exposing (Square, MinePresence(..), CoverPresence(..))


hasMine : Square -> Bool
hasMine square =
    case square of
        ( Mine, _, _ ) ->
            True

        _ ->
            False


uncover : Square -> Square
uncover square =
    case square of
        ( m, Covered, c ) ->
            ( m, Uncovered, c )

        _ ->
            square


isSolved : Square -> Bool
isSolved square =
    case square of
        ( Empty, Uncovered, _ ) ->
            True

        ( Mine, Covered, _ ) ->
            True

        ( Mine, Marked, _ ) ->
            True

        _ ->
            False


isFailed : Square -> Bool
isFailed square =
    case square of
        ( Mine, Uncovered, _ ) ->
            True

        _ ->
            False


toggleMark : Square -> Square
toggleMark square =
    case square of
        ( m, Covered, c ) ->
            ( m, Marked, c )

        ( m, Marked, c ) ->
            ( m, Covered, c )

        _ ->
            square
