module Year exposing (Year, fromDate, view)

import AbsoluteMonth exposing (AbsoluteMonth)
import Calendar exposing (Date)
import CalendarEvent exposing (CalendarEvent, Event, EventDetails(..), TripTime)
import Color
import Date
import Day exposing (Day)
import Debug exposing (log)
import Html exposing (Html, button, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Month exposing (Month)
import Time
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import ViewHelpers exposing (YearDirection(..), YearViewFacts, arcLabel)


type alias Year =
    { days : List Day
    , firstDay : Day
    , months : List Month
    , daysInRange : Int
    , jan1offset : Int
    }


fromDate : Date -> Year
fromDate start =
    let
        end =
            Calendar.incrementYear (Calendar.decrementDay start)

        jan1 =
            Calendar.fromRawParts
                { year = Calendar.getYear start
                , month = Time.Jan
                , day = 1
                }
                |> Maybe.withDefault Date.nullDate

        _ =
            log "jan1" jan1

        jan1offset =
            Calendar.getDayDiff jan1 start

        _ =
            log "jan1offset" jan1offset

        dates =
            Calendar.getDateRange start end

        -- monthRange (dateToMonth start) (dateToMonth end)
        daysInYear =
            List.length dates

        monthsInYear =
            List.length months

        indexedDateToDay =
            \i date ->
                let
                    day =
                        Day.new date
                in
                { day | index = i, daysInRange = daysInYear }

        days =
            List.indexedMap indexedDateToDay dates

        firstDay =
            case days of
                [] ->
                    Day.nullDay

                firstDay_ :: rest ->
                    firstDay_

        months =
            List.foldl monthFold [] days

        indexedMonthToMonth =
            \i m ->
                { m | index = i, monthsInRange = monthsInYear, daysInRange = daysInYear }
    in
    { days = days
    , firstDay = firstDay
    , months = List.indexedMap indexedMonthToMonth months
    , daysInRange = daysInYear
    , jan1offset = jan1offset
    }


monthFold : Day -> List Month -> List Month
monthFold day months =
    let
        absMonth =
            AbsoluteMonth.fromDate day.date
    in
    case months of
        [] ->
            [ Month.fromIndexAndAbsoluteMonth day.index absMonth ]

        first :: rest ->
            if first.date /= absMonth then
                Month.fromIndexAndAbsoluteMonth day.index absMonth :: { first | endDayIndex = day.index } :: rest

            else
                months


yearTransitionView : YearViewFacts -> Year -> List (Svg msg)
yearTransitionView facts year =
    let
        y =
            year.firstDay.year

        offset =
            year.daysInRange - year.jan1offset

        _ =
            log "jan1offset" year.jan1offset
    in
    [ yearTransitionView_ facts 0 y (y + 1)
    , yearTransitionView_ facts offset (y + 1) y
    ]


yearTransitionView_ : YearViewFacts -> Int -> Int -> Int -> Svg msg
yearTransitionView_ facts index pre post =
    let
        ( thisYearAnchor, nextYearAnchor ) =
            case facts.direction of
                Clockwise ->
                    ( AnchorStart, AnchorEnd )

                AntiClockwise ->
                    ( AnchorEnd, AnchorStart )
    in
    g
        [ transform [ Rotate (facts.dayAngle index) 0 0 ]
        ]
        [ line
            [ x1 (px facts.innerRadius)
            , y1 (px 0)
            , x2 (px facts.fullRadius)
            , y2 (px 0)
            ]
            []
        , arcLabel (facts.radius + 12) thisYearAnchor (String.fromInt pre)
        , arcLabel (facts.radius + 12) nextYearAnchor (String.fromInt post)

        -- , text_ [ x (px facts.fullRadius), y (px 0) ] [ text yearString ]
        ]


view : Float -> Year -> List Event -> Html msg
view fullRadius year events =
    let
        _ =
            log "year" year

        radius =
            fullRadius * 0.9

        dir =
            -- Clockwise
            AntiClockwise

        dirMul =
            case dir of
                Clockwise ->
                    1

                AntiClockwise ->
                    -1

        dayAngle =
            \index -> 360 * ((dirMul * index) |> toFloat) / (year.daysInRange |> toFloat)

        dateAngle : Calendar.Date -> Float
        dateAngle =
            dayAngle << Calendar.getDayDiff year.firstDay.date

        dayRadius =
            \rad -> (2 * pi * rad) / toFloat year.daysInRange / 2

        facts =
            { fullRadius = fullRadius
            , radius = radius
            , diameter = fullRadius * 2
            , dayAngle = dayAngle
            , dateAngle = dateAngle
            , innerRadius = radius * 0.25
            , daysRadius = radius * 1
            , dayRadius = dayRadius
            , daysInRange = year.daysInRange
            , direction = dir
            , firstDate = year.firstDay.date
            }

        angleOffset =
            facts.dayAngle year.jan1offset - 90

        -- facts.dayAngle (year.daysInRange - year.jan1offset)
    in
    svg
        [ width (px facts.diameter)
        , height (px facts.diameter)
        , viewBox 0 0 facts.diameter facts.diameter
        ]
        [ g [ transform [ Translate facts.fullRadius facts.fullRadius, Rotate angleOffset 0 0 ] ]
            -- [ circle
            -- [ cx (px 0)
            -- , cy (px 0)
            -- , r (px facts.radius)
            -- , fill FillNone
            -- , strokeWidth (px 0.5)
            -- , stroke <| Color.black
            -- ]
            -- []
            [ circle
                [ cx (px 0)
                , cy (px 0)
                , r (px facts.innerRadius)
                , fill FillNone
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                []
            , g
                [ class [ "month" ]
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map (Month.view facts) year.months)
            , g
                [ class [ "month" ]
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (yearTransitionView
                    facts
                    year
                )
            , g
                [ class [ "day" ]
                , fill FillNone
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map
                    (Day.view facts)
                    year.days
                )
            , CalendarEvent.view facts events
            ]
        ]
