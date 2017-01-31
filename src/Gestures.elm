module Gestures exposing (..)

import Html.Events exposing (on, onWithOptions)
import Html
import Json.Decode


onTap : msg -> Html.Attribute msg
onTap msg =
    on "gesture.tap" (Json.Decode.succeed msg)


onLongTap : msg -> Html.Attribute msg
onLongTap msg =
    on "gesture.longtap" (Json.Decode.succeed msg)


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    let
        preventOpt =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "contextmenu" preventOpt (Json.Decode.succeed msg)
