module Main exposing (..)

import Html exposing (Html, text, div, h1, table, tr, td)
import Html.App as HtmlApp
import Matrix exposing (Matrix)


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
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Minesweeper" ]
        , mapView model.map
        ]


mapView : Map -> Html Msg
mapView map =
    let
        tableRows =
            List.map mapRowView (Matrix.toList map)
    in
        table [] tableRows


mapRowView : List Square -> Html Msg
mapRowView row =
    tr [] (List.map squareView row)


squareView : Square -> Html Msg
squareView square =
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
        td []
            [ text content
            ]
