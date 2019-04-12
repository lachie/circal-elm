module Main exposing (Day, Model, Month, Msg(..), Year, dateToString, firstOfMonth, getCalendarDate, init, isWeekend, main, monthToString, newDay, nullDate, subscriptions, toCalendarDate, update, view, weekdayToString, yearDayView, yearFromDate, yearView)

import Browser
import Calendar exposing (Date)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Task
import Time


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
    }


yearFromDate : Date -> Year
yearFromDate start =
    let
        end =
            Calendar.incrementYear (Calendar.decrementDay start)

        dates =
            Calendar.getDateRange start end

        months =
            monthRange start end

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

        indexedMonthToMonth =
            \i month ->
                let
                    m =
                        newMonth month
                in
                { m | index = i, monthsInRange = monthsInYear }
    in
    { days = List.indexedMap indexedDateToDay dates
    , months = List.indexedMap indexedMonthToMonth months
    }



-- Day


type alias Day =
    { date : Date
    , index : Int
    , daysInRange : Int
    , firstOfMonth : Bool
    , isWeekend : Bool
    }


newDay : Date -> Day
newDay date =
    { date = date
    , index = 0
    , daysInYear = 0
    , firstOfMonth = firstOfMonth date
    , isWeekend = isWeekend date
    }


type alias AbsoluteMonth =
    ( Int, Month )


type alias Month =
    { date : AbsoluteMonth
    , index : Int
    , monthsInRange : Int
    }


newMonth : AbsoluteMonth -> Month
newMonth am =
    { date = am
    , index = 0
    , monthsInRange = 0
    }


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


monthRange : AbsoluteMonth -> AbsoluteMonth -> List AbsoluteMonth
monthRange start end =
    monthRange_ start end []


monthRange_ : AbsoluteMonth -> AbsoluteMonth -> List AbsoluteMonth -> List AbsoluteMonth
monthRange_ ( startYear, startMonth ) ( endYear, endMonth ) acc =
    if startYear >= endYear && startMonth >= endMonth then
        res

    else if startMonth > 11 then
        [ ( startYear, 1 ) ] :: monthRange ( startYear, startMonth + 1 ) ( endYear, endMonth )

    else
        [ ( startYear, startMonth ) ] :: monthRange ( startYear, startMonth + 1 ) ( endYear, endMonth )


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
        [ div [] [ text (dateToString model.today) ]
        , case model.year of
            Just year ->
                yearView year

            Nothing ->
                text "no year"
        ]


yearView : Year -> Html Msg
yearView year =
    Html.ol [] (List.map yearDayView year.days)


yearDayView : Day -> Html Msg
yearDayView day =
    Html.li
        [ classList
            [ ( "day-first-of-month", day.firstOfMonth )
            , ( "day-weekend", day.isWeekend )
            ]
        ]
        [ text
            (if day.firstOfMonth then
                "F"

             else
                ""
            )
        , text
            (if day.isWeekend then
                "W"

             else
                ""
            )
        , text " "
        , text (dateToString day.date)
        , text " "
        , text (String.fromInt (day.index + 1))
        , text " / "
        , text (String.fromInt day.daysInYear)

        -- , text (String.fromBool day.isWeekend)
        ]
