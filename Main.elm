module Main exposing (..)

import Html exposing (Html, text, div, h1)
import Html.App as HtmlApp


main : Program Never
main =
    HtmlApp.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Minesweeper" ]
        ]
