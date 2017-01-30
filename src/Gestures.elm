module Gestures exposing (..)

import Html.Events exposing (on)
import Html
import Json.Decode


onTap : msg -> Html.Attribute msg
onTap msg =
    on "gesture.tap" (Json.Decode.succeed msg)


onLongTap : msg -> Html.Attribute msg
onLongTap msg =
    on "gesture.longtap" (Json.Decode.succeed msg)
