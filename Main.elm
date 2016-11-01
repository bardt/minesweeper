module Main exposing (..)

import Html exposing (Html, text, div, h1, table, tr, td, button)
import Html.App as HtmlApp
import Html.Events exposing (onClick, onWithOptions)
import Matrix exposing (Matrix, Location)
import Json.Decode as Json
import Random exposing (Generator)
import Random.Array as RandomArray
import Array exposing (Array)


main : Program Never
main =
    HtmlApp.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate NewMap (randomMap initialModel.difficultyLevel) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


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


initialModel : Model
initialModel =
    { map = initialMap
    , gameStatus = Started
    , difficultyLevel = difficultyLevels.beginner
    }


initialMap : Map
initialMap =
    Matrix.fromList []


type alias DifficultyLevel =
    { height : Int, width : Int, mines : Int }


type alias DifficultyLevels =
    { beginner : DifficultyLevel
    , intermediate : DifficultyLevel
    , expert : DifficultyLevel
    }


difficultyLevels : DifficultyLevels
difficultyLevels =
    { beginner = DifficultyLevel 9 9 10
    , intermediate = DifficultyLevel 16 16 40
    , expert = DifficultyLevel 16 30 99
    }


randomMap : DifficultyLevel -> Generator Map
randomMap level =
    let
        count =
            min (level.width * level.height) level.mines

        fillMatrix : Array Location -> Map
        fillMatrix locations =
            let
                locList =
                    Array.toList locations
            in
                Matrix.matrix level.width
                    level.height
                    (\loc ->
                        if List.member loc locList then
                            ( Mine, Covered, 0 )
                        else
                            ( Empty, Covered, 0 )
                    )
    in
        Matrix.matrix level.width level.height (\loc -> loc)
            |> Matrix.flatten
            |> Array.fromList
            |> RandomArray.shuffle
            |> Random.map (Array.slice 0 count)
            |> Random.map fillMatrix
            |> Random.map putCountersIntoSquares


countMines : Map -> Int
countMines m =
    Matrix.flatten m
        |> List.map minesInSquare
        |> List.sum


countMarks : Map -> Int
countMarks m =
    let
        checkMark s =
            case s of
                ( _, Marked, _ ) ->
                    True

                _ ->
                    False
    in
        Matrix.flatten m
            |> List.filter checkMark
            |> List.length


putCountersIntoSquares : Map -> Map
putCountersIntoSquares map =
    let
        countOne : Location -> Int
        countOne location =
            Matrix.get location map
                |> Maybe.map minesInSquare
                |> Maybe.withDefault 0

        countEach : Location -> Square -> Square
        countEach location square =
            case square of
                ( Mine, c, m ) ->
                    ( Mine, c, 0 )

                ( Empty, c, m ) ->
                    ( Empty
                    , c
                    , neighbourLocations location
                        |> List.map countOne
                        |> List.sum
                    )
    in
        Matrix.mapWithLocation countEach map


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


minesInSquare : Square -> Int
minesInSquare square =
    case square of
        ( Mine, _, _ ) ->
            1

        _ ->
            0


squareHasMine : Map -> Location -> Bool
squareHasMine map loc =
    Matrix.get loc map
        |> Maybe.map (\x -> 1 == minesInSquare x)
        |> Maybe.withDefault False


mapIsUncovered : Map -> Bool
mapIsUncovered map =
    let
        isWinning square =
            case square of
                ( Empty, Uncovered, _ ) ->
                    True

                ( Mine, Covered, _ ) ->
                    True

                ( Mine, Marked, _ ) ->
                    True

                _ ->
                    False
    in
        Matrix.flatten map
            |> List.all isWinning


mapHasUncoveredMines : Map -> Bool
mapHasUncoveredMines map =
    let
        mineIsUncovered : Square -> Bool
        mineIsUncovered square =
            case square of
                ( Mine, Uncovered, _ ) ->
                    True

                _ ->
                    False
    in
        Matrix.flatten map
            |> List.any mineIsUncovered


resolveGameStatus : Map -> GameStatus
resolveGameStatus map =
    if mapIsUncovered map then
        Won
    else if mapHasUncoveredMines map then
        Failed
    else
        Started



-- UPDATE


type Msg
    = Uncover Location
    | Mark Location
    | StartNewGame
    | NewMap Map


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        uncoveredMap location =
            if model.gameStatus == Started then
                uncover location model.map
            else
                model.map
    in
        case msg of
            Uncover location ->
                ( { model
                    | map =
                        uncoveredMap location
                    , gameStatus =
                        resolveGameStatus (uncoveredMap location)
                  }
                , Cmd.none
                )

            Mark location ->
                ( { model
                    | map = mark location model.map
                  }
                , Cmd.none
                )

            StartNewGame ->
                ( model
                , Random.generate NewMap (randomMap model.difficultyLevel)
                )

            NewMap map ->
                ( { model
                    | map = map
                    , gameStatus = Started
                  }
                , Cmd.none
                )


uncoverOne : Map -> Location -> Map
uncoverOne m loc =
    let
        conditionalUncover : Square -> Square
        conditionalUncover s =
            case s of
                ( m, Covered, c ) ->
                    ( m, Uncovered, c )

                _ ->
                    s
    in
        Matrix.update loc conditionalUncover m


uncover : Location -> Map -> Map
uncover location map =
    let
        canBeUncovered =
            Matrix.get location map
                |> Maybe.map (\( m, c, _ ) -> c == Covered)
                |> Maybe.withDefault False

        noMinesAround : Bool
        noMinesAround =
            Matrix.get location map
                |> Maybe.map (\( m, _, count ) -> count == 0 && m /= Mine)
                |> Maybe.withDefault False

        nextLocationsToUncover : List Location
        nextLocationsToUncover =
            if noMinesAround then
                neighbourLocations location
            else
                []

        uncoverNext : Map -> Map
        uncoverNext m =
            List.foldl uncover m nextLocationsToUncover
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


mark : Location -> Map -> Map
mark location map =
    let
        toggleMark : Square -> Square
        toggleMark s =
            case s of
                ( m, Covered, c ) ->
                    ( m, Marked, c )

                ( m, Marked, c ) ->
                    ( m, Covered, c )

                _ ->
                    s
    in
        Matrix.update location toggleMark map



-- VIEW


view : Model -> Html Msg
view model =
    let
        minesTotal =
            countMines model.map

        marksLeft =
            minesTotal - countMarks model.map

        baseChildren =
            [ h1 []
                [ text "Minesweeper" ]
            , button [ onClick StartNewGame ]
                [ text "New game" ]
            , div []
                [ div [] [ text <| "Total mines: " ++ toString minesTotal ]
                , div [] [ text <| "Flags left: " ++ toString marksLeft ]
                , mapView model.map
                ]
            ]

        children =
            case model.gameStatus of
                Failed ->
                    baseChildren ++ [ h1 [] [ text "You failed" ] ]

                Won ->
                    baseChildren ++ [ h1 [] [ text "The winner is you" ] ]

                Started ->
                    baseChildren
    in
        div [] children


mapView : Map -> Html Msg
mapView map =
    let
        htmlMap =
            Matrix.mapWithLocation squareView map

        tableRows =
            List.map mapRowView (Matrix.toList htmlMap)
    in
        table [] tableRows


mapRowView : List (Html Msg) -> Html Msg
mapRowView rowHtml =
    tr [] rowHtml


squareView : Location -> Square -> Html Msg
squareView location square =
    let
        content =
            case square of
                ( _, Marked, _ ) ->
                    "🚩"

                ( _, Covered, _ ) ->
                    "◻️"

                ( Mine, Uncovered, _ ) ->
                    "💣"

                ( Empty, Uncovered, count ) ->
                    toString count
    in
        td
            [ onClick (Uncover location)
            , onRightClick (Mark location)
            ]
            [ text content
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
