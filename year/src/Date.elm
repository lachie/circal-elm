module Date exposing (firstOfMonth, indexMonth, isWeekend, julianDayNumber, monthIndex, monthOrd, monthToString, nullDate, toJulianDayNumber, toString, weekdayToString)

import Calendar exposing (Date)
import Time


nullDate =
    Calendar.fromPosix (Time.millisToPosix 0)



-- julianDayNumber yy mm d =
-- let
-- ( y, m ) =
-- if mm < 3 then
-- ( yy - 1 + 8000, mm + 12 )
-- else
-- ( yy + 8000, mm )
-- in


julianDayNumber : Int -> Int -> Int -> Int
julianDayNumber y m d =
    -- (1461 * (y + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4 + d - 32075
    (1461 * (y + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4 + d - 32075


toJulianDayNumber : Date -> Int
toJulianDayNumber d =
    julianDayNumber
        (Calendar.getYear d)
        (1
            + (Calendar.getMonth d
                |> monthIndex
              )
        )
        (Calendar.getDay d)


toString : Date -> String
toString d =
    weekdayToString (Calendar.getWeekday d)
        ++ ", "
        ++ String.fromInt (Calendar.getYear d)
        ++ "-"
        ++ monthToString (Calendar.getMonth d)
        ++ "-"
        ++ String.fromInt (Calendar.getDay d)


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


monthOrd : Time.Month -> Int
monthOrd =
    monthIndex >> (+) 1


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
