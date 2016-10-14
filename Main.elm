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


type alias Model =
    { map : Map
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


initialModel : Model
initialModel =
    { map = initialMap
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
        checkMine : Location -> Int
        checkMine location =
            Matrix.get location map
                |> Maybe.map hasMines
                |> Maybe.withDefault 0

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
                , ( row, col )
                , ( row, col + 1 )
                , ( row + 1, col - 1 )
                , ( row + 1, col )
                , ( row + 1, col + 1 )
                ]

        countEach : Location -> Square -> Square
        countEach location square =
            case square of
                ( Mine, c, m ) ->
                    ( Mine, c, 0 )

                ( Empty, c, m ) ->
                    ( Empty
                    , c
                    , neighbourLocations location
                        |> List.map checkMine
                        |> List.sum
                    )
    in
        Matrix.mapWithLocation countEach map


hasMines : Square -> Int
hasMines square =
    case square of
        ( Mine, _, _ ) ->
            1

        _ ->
            0


type Msg
    = Uncover Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Uncover location ->
            { model | map = uncover model.map location }


uncover : Map -> Location -> Map
uncover map location =
    Matrix.update location (\( m, c, count ) -> ( m, Uncovered, count )) map


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Minesweeper" ]
        , mapView model.map
        ]


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
