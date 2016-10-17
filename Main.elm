module Main exposing (..)

import Html exposing (Html, text, div, h1, table, tr, td)
import Html.App as HtmlApp
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, Location)


main : Program Never
main =
    HtmlApp.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { map : Map
    , gameStatus : GameStatus
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


type GameStatus
    = Started
    | Failed
    | Won


initialModel : Model
initialModel =
    { map = initialMap
    , gameStatus = Started
    }


initialMap : Map
initialMap =
    let
        -- Short for Free
        f =
            ( Empty, Covered, 0 )

        -- Short for Mine
        m =
            ( Mine, Covered, 0 )

        x =
            [ [ f, m, f, f, f ]
            , [ m, f, f, f, m ]
            , [ f, f, f, f, f ]
            , [ m, f, m, f, f ]
            , [ f, f, f, f, f ]
            ]
    in
        countMines (Matrix.fromList x)


countMines : Map -> Map
countMines map =
    let
        countOne : Location -> Int
        countOne location =
            Matrix.get location map
                |> Maybe.map hasMinesCount
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


hasMinesCount : Square -> Int
hasMinesCount square =
    case square of
        ( Mine, _, _ ) ->
            1

        _ ->
            0


hasMine : Map -> Location -> Bool
hasMine map loc =
    Matrix.get loc map
        |> Maybe.map (\x -> 1 == hasMinesCount x)
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


update : Msg -> Model -> Model
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
                { model
                    | map =
                        uncoveredMap location
                    , gameStatus =
                        resolveGameStatus (uncoveredMap location)
                }


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

        uncoverOne : Map -> Location -> Map
        uncoverOne m loc =
            Matrix.update loc (\( m, c, count ) -> ( m, Uncovered, count )) m

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



-- VIEW


view : Model -> Html Msg
view model =
    let
        baseChildren =
            [ h1 [] [ text "Minesweeper" ]
            , mapView model.map
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
                ( _, Covered, _ ) ->
                    "◻️"

                ( Mine, Uncovered, _ ) ->
                    "💣"

                ( Empty, Uncovered, count ) ->
                    toString count
    in
        td
            [ onClick (Uncover location)
            ]
            [ text content
            ]
