module Day exposing (Day, new, nullDay, view)

import Calendar exposing (Date)
import Color
import Date
import Html exposing (Html, button, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Time
import TypedSvg exposing (circle, g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import ViewHelpers exposing (ViewFacts)


type alias Day =
    { date : Date
    , year : Int
    , month : Time.Month
    , dayOfMonth : Int
    , dayOfWeek : Time.Weekday
    , index : Int
    , daysInRange : Int
    }


nullDay =
    { date = Date.nullDate
    , year = 0
    , month = Time.Jan
    , dayOfMonth = 0
    , dayOfWeek = Time.Mon
    , index = 0
    , daysInRange = 0
    }


ratio lo hi count x =
    if hi >= lo then
        lo + (x / count) * (hi - lo)

    else
        hi + (1 - (x / count)) * (lo - hi)


dayColour : Int -> Day -> Color.Color
dayColour year day =
    let
        mi =
            Date.monthIndex day.month |> toFloat

        hue =
            mi / 12

        lightness =
            if day.year == year then
                ratio 0.5 0.9 5

            else
                ratio 0.6 1.0 5

        col =
            \i -> Color.hsl hue 0.8 (lightness i)
    in
    case day.dayOfWeek of
        Time.Mon ->
            col 0

        Time.Tue ->
            col 1

        Time.Wed ->
            col 2

        Time.Thu ->
            col 3

        Time.Fri ->
            col 4

        Time.Sat ->
            Color.lightGrey

        Time.Sun ->
            Color.grey


new : Date -> Day
new date =
    { date = date
    , year = Calendar.getYear date
    , month = Calendar.getMonth date
    , dayOfMonth = Calendar.getDay date
    , dayOfWeek = Calendar.getWeekday date
    , index = 0
    , daysInRange = 0
    }


weekend : Day -> Bool
weekend { dayOfWeek } =
    case dayOfWeek of
        Time.Sat ->
            True

        Time.Sun ->
            True

        _ ->
            False


view : ViewFacts -> Day -> Svg msg
view facts day =
    let
        radius =
            facts.radiusN 0

        dayRadius =
            facts.dayRadiusN 0

        thisDayAngle =
            facts.dayAngleIndex day.index

        halfOneDayAngle =
            0.5 * facts.dayAngleIndex 1

        colour =
            dayColour facts.year day
    in
    g []
        -- [ g
        -- [ transform [ Rotate (thisDayAngle - halfOneDayAngle) 0 0, Translate radius 0 ]
        -- ]
        -- [ line
        -- [ x1 (px <| dayRadius * -0.2)
        -- , y1 (px 0)
        -- , x2 (px <| dayRadius * 1.4)
        -- , y2 (px 0)
        -- ]
        -- []
        -- ]
        [ g
            [ transform [ Rotate thisDayAngle 0 0, Translate radius 0 ]
            ]
            [ circle
                [ fill <| Fill colour
                , strokeWidth (px 0)
                , cx (px <| dayRadius / 2)
                , cy (px 0)
                , r (px dayRadius)
                ]
                []
            , text_
                [ transform [ Rotate 90 0 0 ]
                , fontSize (px (1.2 * dayRadius))
                , textAnchor AnchorMiddle
                ]
                [ text (String.fromInt day.dayOfMonth) ]
            ]
        ]
