module ViewHelpers exposing (YearDirection(..), YearViewFacts, arcLabel)

import Calendar
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)


type YearDirection
    = Clockwise
    | AntiClockwise


type alias YearViewFacts =
    { dayAngle : Int -> Float
    , dateAngle : Calendar.Date -> Float
    , fullRadius : Float
    , radius : Float
    , diameter : Float
    , innerRadius : Float
    , daysRadius : Float
    , dayRadius : Float -> Float
    , daysInRange : Int
    , direction : YearDirection
    , firstDate : Calendar.Date
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
