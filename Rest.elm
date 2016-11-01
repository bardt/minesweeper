module Rest exposing (..)


boolToInt : Bool -> Int
boolToInt condition =
    if condition then
        1
    else
        0
