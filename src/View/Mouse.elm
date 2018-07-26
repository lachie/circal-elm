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
            newMessage
            positionDecoder


positionDecoder : Json.Decoder Point
positionDecoder =
    Json.map2 Point
        (Json.at [ "offsetX" ] Json.float)
        (Json.at [ "offsetY" ] Json.float)


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
