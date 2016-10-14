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
    ( MinePresence, CoverPresence )


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
            ( Empty, Covered )

        -- Short for Mine
        m =
            ( Mine, Covered )

        x =
            [ [ f, m, f, f, f ]
            , [ m, f, f, f, m ]
            , [ f, f, f, f, f ]
            , [ m, f, m, f, f ]
            , [ f, f, f, f, f ]
            ]
    in
        Matrix.fromList x


type Msg
    = Uncover Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Uncover location ->
            { model | map = uncover model.map location }


uncover : Map -> Location -> Map
uncover map location =
    Matrix.update location (\( m, c ) -> ( m, Uncovered )) map


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
                ( _, Covered ) ->
                    "◻️"

                ( Mine, Uncovered ) ->
                    "💣"

                ( Empty, Uncovered ) ->
                    "1"
    in
        td
            [ onClick (Uncover location)
            ]
            [ text content
            ]
