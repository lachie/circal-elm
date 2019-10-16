module Year exposing (Year, dayRadiusN, fromDate, radiusN, view)

import AbsoluteMonth exposing (AbsoluteMonth)
import Calendar exposing (Date)
import CalendarEvent exposing (Evt)
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
import ViewHelpers exposing (ViewFacts, YearDirection(..), arcLabel)


type alias Year =
    { days : List Day
    , firstDay : Day
    , months : List Month
    , daysInRange : Int
    , jan1offset : Int
    , year : Int
    }


type alias YearViewFacts =
    { viewFacts : ViewFacts
    , fullRadius : Float
    , radius : Float
    , diameter : Float
    , innerRadius : Float
    , daysInRange : Int
    }


radiusN : Int -> Float -> Int -> Float
radiusN daysInYear seedRadius iter =
    case iter of
        0 ->
            seedRadius

        n ->
            let
                dir =
                    iter // abs iter

                dirF =
                    toFloat dir

                dayDiameter =
                    dirF * 2 * pi * seedRadius / (daysInYear |> toFloat)
            in
            radiusN daysInYear (seedRadius - dayDiameter) (n - dir)


dayRadiusN : Int -> Float -> Int -> Float
dayRadiusN daysInYear seedRadius iter =
    let
        radiusAround =
            radiusN daysInYear seedRadius iter
    in
    2 * pi * radiusAround / (daysInYear |> toFloat) / 2.0


fromDate : Date -> Year
fromDate start =
    let
        end =
            Calendar.incrementYear (Calendar.decrementDay start)

        currentYear =
            Calendar.getYear start

        jan1 =
            Calendar.fromRawParts
                { year = currentYear
                , month = Time.Jan
                , day = 1
                }
                |> Maybe.withDefault Date.nullDate

        jan1offset =
            Calendar.getDayDiff jan1 start

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
    , year = currentYear
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


view : Float -> Year -> List Evt -> Html msg
view fullRadius year events =
    let
        -- _ =
        -- log "year" year
        radius =
            fullRadius * 0.9

        _ =
            log "radius" radius

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
            360 / (year.daysInRange |> toFloat)

        dayAngleAtIndex =
            \index -> dayAngle * ((dirMul * index) |> toFloat)

        dateAngle : Calendar.Date -> Float
        dateAngle =
            dayAngleAtIndex << Calendar.getDayDiff year.firstDay.date

        startJDN =
            Date.toJulianDayNumber year.firstDay.date

        endJDN =
            startJDN + year.daysInRange

        viewFacts =
            { dayAngleIndex = dayAngleAtIndex
            , dayAngleJDN = \x -> dayAngleAtIndex (x - startJDN)
            , dayAngle = dayAngle
            , seedRadius = radius
            , startJDN = startJDN
            , endJDN = endJDN
            , year = year.year
            , jdnInYear = \j -> (j >= startJDN) && (j <= endJDN)
            , direction = dir
            , dayRadiusN = dayRadiusN year.daysInRange radius
            , radiusN = radiusN year.daysInRange radius
            }

        facts =
            { viewFacts = viewFacts
            , fullRadius = fullRadius
            , radius = radius
            , diameter = fullRadius * 2
            , innerRadius = radius * 0.25
            , daysInRange = year.daysInRange
            }

        angleOffset =
            viewFacts.dayAngleIndex year.jan1offset - 90

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
            -- [ circle
            -- -- middle circle
            -- [ cx (px 0)
            -- , cy (px 0)
            -- , r (px facts.innerRadius)
            -- , fill FillNone
            -- , stroke <| Color.black
            -- , strokeWidth (px 0.5)
            -- ]
            -- []
            [ g
                -- month slices
                [ class [ "month" ]
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map (Month.view facts) year.months)
            , g
                [ class [ "day" ]
                , fill FillNone
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map
                    (Day.view facts.viewFacts)
                    year.days
                )
            , CalendarEvent.view facts.viewFacts events
            ]
        ]
