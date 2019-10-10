module ViewHelpers exposing (ViewFacts, YearDirection(..), arcLabel)

import Calendar
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)


type YearDirection
    = Clockwise
    | AntiClockwise


type alias ViewFacts =
    { startJDN : Int
    , endJDN : Int
    , jdnInYear : Int -> Bool
    , direction : YearDirection
    , seedRadius : Float
    , dayRadiusN : Int -> Float
    , radiusN : Int -> Float
    , dayAngleJDN : Int -> Float
    , dayAngleIndex : Int -> Float
    , dayAngle : Float
    }


arcLabel : Float -> AnchorAlignment -> String -> Svg msg
arcLabel radius anchor t =
    text_
        [ fontSize (px 12)
        , x (px 0)
        , y (px 0)
        , textAnchor anchor
        , transform [ Translate radius 0, Rotate 90 0 0 ]
        ]
        [ text t ]
