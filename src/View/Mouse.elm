module View.Mouse exposing (..)

import Json.Decode as Json
import TypedSvg.Events exposing (on)
import TypedSvg.Core exposing (Attribute)


type alias Point =
    { x : Float
    , y : Float
    }


onMouseMove : (Point -> msg) -> Attribute msg
onMouseMove newMessage =
    on "mousemove" <|
        Json.map
            (positionInCanvas >> newMessage)
            (traceDecoder "mm" positionDecoder)


positionInCanvas : ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> Point
positionInCanvas ( client, offset, body, documentElement ) =
    let
        ( cx, cy ) =
            client

        ( ox, oy ) =
            offset

        ( bx, by ) =
            body

        ( dx, dy ) =
            documentElement
    in
        Point ((cx + bx + dx) - ox) ((cy + by + dy) - oy)


positionDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
positionDecoder =
    Json.map4 (,,,)
        (toTuple [ "clientX" ] [ "clientY" ])
        (toTuple [ "offsetX" ] [ "offsetY" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 (,) (Json.at x Json.float) (Json.at y Json.float)


traceDecoder : String -> Json.Decoder msg -> Json.Decoder msg
traceDecoder logMessage decoder =
    Json.value
        |> Json.andThen
            (\value ->
                case Json.decodeValue decoder value of
                    Ok decoded ->
                        Json.succeed <| Debug.log "decoded" <| decoded

                    Err err ->
                        Json.fail <| Debug.log logMessage <| err
            )
