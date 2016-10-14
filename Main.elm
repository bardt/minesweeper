module Main exposing (..)

import Html exposing (Html, text, div, h1)
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
    { hasMine : Bool
    , isCovered : Bool
    }


initialModel : Model
initialModel =
    { map = initialMap
    }


initialMap : Map
initialMap =
    let
        -- Short for Free
        f =
            { hasMine = False
            , isCovered = True
            }

        -- Short for Mine
        m =
            { hasMine = True
            , isCovered = True
            }

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
        ]
