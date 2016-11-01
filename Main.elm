module Main exposing (..)

import Types exposing (..)
import Square
import Html exposing (Html, text, div, h1, table, tr, td, button)
import Html.App as HtmlApp
import Html.Events exposing (onClick, onWithOptions)
import Matrix exposing (Matrix, Location)
import Json.Decode as Json
import Random exposing (Generator)
import Map
import Rest exposing (..)


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
    ( initialModel, Random.generate NewMap (Map.random initialModel.difficultyLevel) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


initialModel : Model
initialModel =
    { map = Map.empty
    , gameStatus = Started
    , difficultyLevel = difficultyLevels.expert
    }


difficultyLevels : DifficultyLevels
difficultyLevels =
    { beginner = DifficultyLevel 9 9 10
    , intermediate = DifficultyLevel 16 16 40
    , expert = DifficultyLevel 16 30 99
    }


countMinesTotal : Map -> Int
countMinesTotal m =
    Matrix.flatten m
        |> List.map (boolToInt << Square.hasMine)
        |> List.sum


countUsedMarks : Map -> Int
countUsedMarks m =
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


checkGameStatus : Map -> GameStatus
checkGameStatus map =
    if Map.isSolved map then
        Won
    else if Map.isFailed map then
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
                Map.uncover model.map location
            else
                model.map
    in
        case msg of
            Uncover location ->
                ( { model
                    | map =
                        uncoveredMap location
                    , gameStatus =
                        checkGameStatus (uncoveredMap location)
                  }
                , Cmd.none
                )

            Mark location ->
                ( { model
                    | map = Map.mark model.map location
                  }
                , Cmd.none
                )

            StartNewGame ->
                ( model
                , Random.generate NewMap (Map.random model.difficultyLevel)
                )

            NewMap map ->
                ( { model
                    | map = map
                    , gameStatus = Started
                  }
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    let
        minesTotal =
            countMinesTotal model.map

        marksLeft =
            minesTotal - countUsedMarks model.map

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
