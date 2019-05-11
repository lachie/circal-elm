module Month exposing (Month, fromIndexAndAbsoluteMonth, view)

import AbsoluteMonth exposing (AbsoluteMonth)
import Date exposing (monthToString)
import Html exposing (Html, button, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import ViewHelpers exposing (ViewFacts, YearDirection(..), arcLabel)


type alias Month =
    { date : AbsoluteMonth
    , index : Int
    , startDayIndex : Int
    , endDayIndex : Int
    , monthsInRange : Int
    , daysInRange : Int
    }


type alias YearViewFacts a =
    { a
        | viewFacts : ViewFacts
        , innerRadius : Float
        , radius : Float
    }


fromIndexAndAbsoluteMonth : Int -> AbsoluteMonth -> Month
fromIndexAndAbsoluteMonth index absMonth =
    { startDayIndex = index, endDayIndex = 0, date = absMonth, index = 0, monthsInRange = 0, daysInRange = 0 }


view : YearViewFacts a -> Month -> Svg msg
view facts month =
    let
        ( _, m ) =
            month.date

        anchor =
            case facts.viewFacts.direction of
                Clockwise ->
                    AnchorStart

                AntiClockwise ->
                    AnchorEnd
    in
    g
        [ transform [ Rotate (facts.viewFacts.dayAngleIndex month.startDayIndex) 0 0 ]
        ]
        [ line
            [ x1 (px facts.innerRadius)
            , y1 (px 0)
            , x2 (px facts.radius)
            , y2 (px 0)
            ]
            []
        , arcLabel (facts.viewFacts.daysRadiusN 0) anchor (monthToString m)
        ]
