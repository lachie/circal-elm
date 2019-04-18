module Main exposing (Day, Model, Month, Msg(..), Year, dateToString, firstOfMonth, getCalendarDate, init, isWeekend, main, monthToString, newDay, nullDate, subscriptions, toCalendarDate, update, view, weekdayToString, yearFromDate, yearView)

import Browser
import Calendar exposing (Date)
import Color
import Debug exposing (log)
import Html exposing (Html, button, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Task
import Time
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


nullDate =
    Calendar.fromPosix (Time.millisToPosix 0)



-- MODEL
-- Year


type alias Year =
    { days : List Day
    , months : List Month
    , daysInRange : Int
    , jan1offset : Int
    }


yearFromDate : Date -> Year
yearFromDate start =
    let
        end =
            Calendar.incrementYear (Calendar.decrementDay start)

        jan1 =
            Calendar.fromRawParts
                { year = Calendar.getYear start
                , month = Calendar.getMonth start
                , day = 1
                }
                |> Maybe.withDefault nullDate

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
                        newDay date
                in
                { day | index = i, daysInRange = daysInYear }

        days =
            List.indexedMap indexedDateToDay dates

        months =
            List.foldl monthFold [] days

        indexedMonthToMonth =
            \i m ->
                { m | index = i, monthsInRange = monthsInYear, daysInRange = daysInYear }
    in
    { days = days
    , months = List.indexedMap indexedMonthToMonth months
    , daysInRange = daysInYear
    , jan1offset = jan1offset
    }


monthFold : Day -> List Month -> List Month
monthFold day months =
    let
        absMonth =
            dateToMonth day.date

        _ =
            log "monthFold" "----"

        _ =
            log "day" day

        _ =
            log "mf" absMonth

        _ =
            log "ms" months
    in
    case months of
        [] ->
            [ newMonthIndex day.index absMonth ]

        first :: rest ->
            if first.date /= absMonth then
                newMonthIndex day.index absMonth :: { first | endDayIndex = day.index } :: rest

            else
                months



-- Day


type alias Day =
    { date : Date
    , year : Int
    , month : Time.Month
    , dayOfMonth : Int
    , index : Int
    , daysInRange : Int
    , firstOfMonth : Bool
    , isWeekend : Bool
    }


newDay : Date -> Day
newDay date =
    { date = date
    , year = Calendar.getYear date
    , month = Calendar.getMonth date
    , dayOfMonth = Calendar.getDay date
    , index = 0
    , daysInRange = 0
    , firstOfMonth = firstOfMonth date
    , isWeekend = isWeekend date
    }


type alias AbsoluteMonth =
    ( Int, Time.Month )


absMonthToString : AbsoluteMonth -> String
absMonthToString ( y, m ) =
    String.fromInt y ++ "," ++ monthToString m


type alias Month =
    { date : AbsoluteMonth
    , index : Int
    , startDayIndex : Int
    , endDayIndex : Int
    , monthsInRange : Int
    , daysInRange : Int
    }


newMonthIndex : Int -> AbsoluteMonth -> Month
newMonthIndex index absMonth =
    { startDayIndex = index, endDayIndex = 0, date = absMonth, index = 0, monthsInRange = 0, daysInRange = 0 }


firstOfMonth : Date -> Bool
firstOfMonth =
    (==) 1 << Calendar.getDay


isWeekend : Date -> Bool
isWeekend date =
    case Calendar.getWeekday date of
        Time.Sat ->
            True

        Time.Sun ->
            True

        _ ->
            False


monthToString : Time.Month -> String
monthToString m =
    case m of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"



-- nextMonth : Time.Month -> Time.Month
-- nextMonth m =
-- case m of
-- Time.Jan ->
-- Time.Feb
-- Time.Feb ->
-- Time.Mar
-- Time.Mar ->
-- Time.Apr
-- Time.Apr ->
-- Time.May
-- Time.May ->
-- Time.Jun
-- Time.Jun ->
-- Time.Jul
-- Time.Jul ->
-- Time.Aug
-- Time.Aug ->
-- Time.Sep
-- Time.Sep ->
-- Time.Oct
-- Time.Oct ->
-- Time.Nov
-- Time.Nov ->
-- Time.Dec
-- Time.Dec ->
-- Time.Jan


weekdayToString : Time.Weekday -> String
weekdayToString w =
    case w of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthIndex : Time.Month -> Int
monthIndex m =
    case m of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11


indexMonth : Int -> Time.Month
indexMonth i =
    case i of
        0 ->
            Time.Jan

        1 ->
            Time.Feb

        2 ->
            Time.Mar

        3 ->
            Time.Apr

        4 ->
            Time.May

        5 ->
            Time.Jun

        6 ->
            Time.Jul

        7 ->
            Time.Aug

        8 ->
            Time.Sep

        9 ->
            Time.Oct

        10 ->
            Time.Nov

        11 ->
            Time.Dec

        _ ->
            Time.Dec


dateToMonth : Date -> AbsoluteMonth
dateToMonth d =
    ( Calendar.getYear d, Calendar.getMonth d )


monthRange : AbsoluteMonth -> AbsoluteMonth -> List AbsoluteMonth
monthRange ( startYear, startMonth ) ( endYear, endMonth ) =
    let
        startMonthIndex =
            monthIndex startMonth

        endMonthIndex =
            monthIndex startMonth
    in
    monthRange_ startYear startMonthIndex endYear endMonthIndex [ ( startYear, startMonth ) ]


nextYearMonth : Int -> Int -> ( Int, Int )
nextYearMonth y m =
    let
        nextM =
            m + 1
    in
    if nextM > 11 then
        ( y + 1, 0 )

    else
        ( y, nextM )


monthRange_ : Int -> Int -> Int -> Int -> List AbsoluteMonth -> List AbsoluteMonth
monthRange_ startYear startMonth endYear endMonth acc =
    let
        ( year, month ) =
            nextYearMonth startYear startMonth
    in
    if year >= endYear && month > endMonth then
        acc

    else
        monthRange_ year month endYear endMonth (acc ++ [ ( year, indexMonth month ) ])


type alias Model =
    { today : Date
    , year : Maybe Year
    }


getCalendarDate : Task.Task x Date
getCalendarDate =
    Task.map2 toCalendarDate Time.here Time.now


toCalendarDate : Time.Zone -> Time.Posix -> Date
toCalendarDate zone now =
    let
        rawDate =
            { day = Time.toDay zone now
            , month = Time.toMonth zone now
            , year = Time.toYear zone now
            }
    in
    Calendar.fromRawParts rawDate |> Maybe.withDefault nullDate


init : () -> ( Model, Cmd Msg )
init _ =
    ( { today = nullDate
      , year = Nothing
      }
    , Task.perform SetToday getCalendarDate
    )



-- UPDATE


type Msg
    = SetToday Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetToday today ->
            ( { model | today = today, year = Just (yearFromDate today) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


dateToString : Date -> String
dateToString d =
    weekdayToString (Calendar.getWeekday d)
        ++ ", "
        ++ String.fromInt (Calendar.getYear d)
        ++ "-"
        ++ monthToString (Calendar.getMonth d)
        ++ "-"
        ++ String.fromInt (Calendar.getDay d)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.text (dateToString model.today) ]
        , case model.year of
            Just year ->
                yearView 450 year

            Nothing ->
                Html.text "no year"
        ]


type alias YearViewFacts =
    { dayAngle : Int -> Float
    , fullRadius : Float
    , radius : Float
    , diameter : Float
    , innerRadius : Float
    , dayRadius : Float
    }


yearView : Float -> Year -> Html Msg
yearView fullRadius year =
    let
        _ =
            log "year" year

        radius =
            fullRadius * 0.9

        facts =
            { fullRadius = fullRadius
            , radius = radius
            , diameter = fullRadius * 2
            , dayAngle = \index -> 360 * (index |> toFloat) / (year.daysInRange |> toFloat)
            , innerRadius = radius * 0.1
            , dayRadius = radius * 1
            }

        angleOffset =
            facts.dayAngle year.jan1offset
    in
    svg
        [ width (px facts.diameter)
        , height (px facts.diameter)
        , viewBox 0 0 facts.diameter facts.diameter
        ]
        [ g [ transform [ Translate facts.fullRadius facts.fullRadius, Rotate angleOffset 0 0 ] ]
            [ circle
                [ cx (px 0)
                , cy (px 0)
                , r (px facts.radius)
                , fill FillNone
                , strokeWidth (px 0.5)
                , stroke <| Color.black
                ]
                []
            , circle
                [ cx (px 0)
                , cy (px 0)
                , r (px facts.innerRadius)
                , fill FillNone
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                []
            , g
                [ class [ "day" ]
                , fill FillNone
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map
                    (dayView facts)
                    year.days
                )
            , g
                [ class [ "month" ]
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (List.map (monthView facts) year.months)
            , g
                [ class [ "month" ]
                , stroke <| Color.black
                , strokeWidth (px 0.5)
                ]
                (yearTransitionView
                    facts
                    year
                )
            ]
        ]


yearTransitionView : YearViewFacts -> Year -> List (Svg Msg)
yearTransitionView facts year =
    let
        y =
            case year.days of
                [] ->
                    0

                firstDay :: rest ->
                    firstDay.year
    in
    [ line [ x1 (px 0), y1 (px 0), x2 (px facts.fullRadius), y2 (px 0) ] []
    , arcLabel (facts.radius + 12) AnchorStart (String.fromInt y)
    , arcLabel (facts.radius + 12) AnchorEnd (String.fromInt <| y + 1)

    -- , text_ [ x (px facts.fullRadius), y (px 0) ] [ text yearString ]
    ]


dayView : YearViewFacts -> Day -> Svg Msg
dayView facts day =
    let
        radius =
            facts.dayRadius

        circ =
            2 * pi * radius

        dayRadius =
            circ / toFloat day.daysInRange / 2
    in
    circle
        [ cx (px 0)
        , cy (px 0)
        , r (px dayRadius)
        , transform [ Rotate (facts.dayAngle day.index) 0 0, Translate (radius - dayRadius) 0 ]
        ]
        []


monthView : YearViewFacts -> Month -> Svg Msg
monthView facts month =
    let
        ( _, m ) =
            month.date
    in
    g
        [ transform [ Rotate (facts.dayAngle month.startDayIndex) 0 0 ]
        ]
        [ line
            [ x1 (px 0)
            , y1 (px 0)
            , x2 (px facts.radius)
            , y2 (px 0)
            ]
            []
        , arcLabel facts.radius AnchorStart (monthToString m)
        ]


arcLabel : Float -> AnchorAlignment -> String -> Svg Msg
arcLabel radius anchor t =
    text_
        [ fontSize (px 12)
        , x (px 0)
        , y (px 0)
        , textAnchor anchor
        , transform [ Translate radius 0, Rotate 90 0 0 ]
        ]
        [ text t ]



-- div
-- []
-- [ Html.ol []
-- (List.map yearDayView year.days)
-- , Html.ol
-- []
-- (List.map yearMonthView year.months)
-- ]
-- yearMonthView : Month -> Html Msg
-- yearMonthView month =
-- let
-- ( year, m ) =
-- month.date
-- in
-- Html.li []
-- [ text (String.fromInt year)
-- , text " "
-- , text (monthToString m)
-- ]
-- yearDayView : Day -> Html Msg
-- yearDayView day =
-- Html.li
-- [ classList
-- [ ( "day-first-of-month", day.firstOfMonth )
-- , ( "day-weekend", day.isWeekend )
-- ]
-- ]
-- [ text
-- (if day.firstOfMonth then
-- "F"
-- else
-- ""
-- )
-- , text
-- (if day.isWeekend then
-- "W"
-- else
-- ""
-- )
-- , text " "
-- , text (dateToString day.date)
-- , text " "
-- , text (String.fromInt (day.index + 1))
-- , text " / "
-- , text (String.fromInt day.daysInRange)
-- -- , text (String.fromBool day.isWeekend)
-- ]
