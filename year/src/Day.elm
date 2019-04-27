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
import ViewHelpers exposing (YearViewFacts)


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


view : YearViewFacts -> Day -> Svg msg
view facts day =
    let
        radius =
            facts.daysRadius

        dayRadius =
            facts.dayRadius radius

        dayFill =
            if weekend day then
                Fill Color.gray

            else
                Fill Color.white
    in
    circle
        [ cx (px 0)
        , cy (px 0)
        , r (px dayRadius)
        , transform [ Rotate (facts.dayAngle day.index) 0 0, Translate (radius - dayRadius) 0 ]
        , fill dayFill
        ]
        [ title [] [ text (Date.toString day.date) ] ]
