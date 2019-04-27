module AbsoluteMonth exposing (AbsoluteMonth, absMonthToString, fromDate)

import Calendar exposing (Date)
import Date exposing (monthToString)
import Time


type alias AbsoluteMonth =
    ( Int, Time.Month )


absMonthToString : AbsoluteMonth -> String
absMonthToString ( y, m ) =
    String.fromInt y ++ "," ++ monthToString m


fromDate : Date -> AbsoluteMonth
fromDate d =
    ( Calendar.getYear d, Calendar.getMonth d )
