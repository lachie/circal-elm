module Year exposing (Year, dayRadiusN, daysRadiusN, fromDate, view)

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
    }


type alias YearViewFacts =
    { viewFacts : ViewFacts
    , fullRadius : Float
    , radius : Float
    , diameter : Float
    , innerRadius : Float
    , daysInRange : Int
    }


daysRadiusN : Int -> Float -> Int -> Float
daysRadiusN daysInYear seedRadius iter =
    case iter of
        0 ->
            seedRadius

        n ->
            let
                dayDiameter =
                    2 * pi * seedRadius / (daysInYear |> toFloat)
            in
            daysRadiusN daysInYear (seedRadius - dayDiameter) (n - 1)


dayRadiusN : Int -> Float -> Int -> Float
dayRadiusN daysInYear seedRadius iter =
    let
        daysRadius =
            daysRadiusN daysInYear seedRadius iter
    in
    2 * pi * daysRadius / (daysInYear |> toFloat) / 2.0


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
            case facts.viewFacts.direction of
                Clockwise ->
                    ( AnchorStart, AnchorEnd )

                AntiClockwise ->
                    ( AnchorEnd, AnchorStart )
    in
    g
        [ transform [ Rotate (facts.viewFacts.dayAngleIndex index) 0 0 ]
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
            \index -> 360 * ((dirMul * index) |> toFloat) / (year.daysInRange |> toFloat)

        dateAngle : Calendar.Date -> Float
        dateAngle =
            dayAngle << Calendar.getDayDiff year.firstDay.date

        startJDN =
            Date.toJulianDayNumber year.firstDay.date

        viewFacts =
            { dayAngleIndex = dayAngle
            , dayAngleJDN = \x -> dayAngle (x - startJDN)
            , seedRadius = radius
            , startJDN = startJDN
            , endJDN = startJDN + year.daysInRange
            , direction = dir
            , dayRadiusN = dayRadiusN year.daysInRange radius
            , daysRadiusN = daysRadiusN year.daysInRange radius
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
                    (Day.view facts.viewFacts)
                    year.days
                )
            , CalendarEvent.view facts.viewFacts events
            ]
        ]
