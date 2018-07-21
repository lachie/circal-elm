module Types exposing (..)

import Date exposing (Date, Month(..))
import Date.Extra exposing (fromCalendarDate, add)
import Time exposing (Time, millisecond, hour)
import Mouse
import View.Mouse exposing (Point)


type Msg
    = SelectDay Day
    | YearAngle Float
    | YearMove PolarPoint
    | DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position


type alias PolarPoint =
    { r : Float
    , t : Float
    }


type alias Event =
    { title : String }


type alias Model =
    { year : Year
    , yearAngle : Float
    , selectedDay : Maybe Day
    }


type alias Year =
    { year : Int
    , dayOneDate : Date
    , days : List Day
    }


type alias Day =
    { index : Int
    , month : Int
    , dayOfMonth : Int
    , dayOfWeek : Int
    , date : Date
    }


yearForDate : Date -> Year
yearForDate date =
    let
        year =
            Date.year date

        len =
            yearLength year

        dayOneDate =
            Date.Extra.fromCalendarDate year Jan 1
    in
        Year year dayOneDate (dayRange dayOneDate len)


yearForMilliseconds : Float -> Year
yearForMilliseconds =
    ((*) Time.millisecond) >> Date.fromTime >> yearForDate


isLeapYear : Int -> Bool
isLeapYear year =
    let
        divisibleBy n y =
            rem y n == 0
    in
        xor (divisibleBy 4 year) (divisibleBy 100 year) || (divisibleBy 400 year)


yearLength : Int -> Int
yearLength year =
    if isLeapYear year then
        366
    else
        365


dayRange : Date -> Int -> List Day
dayRange baseDate length =
    List.range 0 (length - 1) |> List.map (dayAfterBase baseDate)


dayAfterBase : Date -> Int -> Day
dayAfterBase baseDate i =
    let
        date =
            dateAfterBase baseDate i

        month =
            Date.Extra.monthNumber date - 1

        dayOfMonth =
            Date.day date - 1

        dayOfWeek =
            Date.Extra.weekdayNumber date - 1
    in
        Day i month dayOfMonth dayOfWeek date


dateAfterBase : Date -> Int -> Date
dateAfterBase baseDate i =
    Date.Extra.add Date.Extra.Day i baseDate


dayIsMonthStart : Day -> Bool
dayIsMonthStart { dayOfMonth } =
    dayOfMonth == 0


dayIsWeekend : Day -> Bool
dayIsWeekend { dayOfWeek } =
    dayOfWeek == 5 || dayOfWeek == 6
