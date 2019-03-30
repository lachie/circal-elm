import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Time
import Task
import Calendar exposing (Date)


main =
  Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions 
  }

nullDate = Calendar.fromPosix (Time.millisToPosix 0)

-- MODEL

-- Year

type alias Year = {
  days: List Day
  }

yearFromDate : Date -> Year
yearFromDate start =
  let
      end = (Calendar.incrementYear (Calendar.decrementDay start))
      dates = Calendar.getDateRange start end
      daysInYear = List.length dates
      indexedDateToDay = (\i date -> 
        let 
            day = (newDay date)
        in
            { day | index=i, daysInYear=daysInYear }
            )
  in
      { days  = List.indexedMap indexedDateToDay dates
      }

-- Day

type alias Day = 
  { date: Date
  , index: Int
  , daysInYear: Int
  , firstOfMonth: Bool
  , isWeekend: Bool
  }

newDay : Date -> Day
newDay date = 
  { date = date
  , index = 0
  , daysInYear = 0
  , firstOfMonth = (firstOfMonth date)
  , isWeekend = (isWeekend date)
  }

firstOfMonth : Date -> Bool
firstOfMonth = (==) 1 << Calendar.getDay

isWeekend : Date -> Bool
isWeekend date =
  case (Calendar.getWeekday date) of
    Time.Sat -> True
    Time.Sun -> True
    _ -> False

monthToString : Time.Month -> String
monthToString m =
  case m of
    Time.Jan -> "January"
    Time.Feb -> "February"
    Time.Mar -> "March"
    Time.Apr -> "April"
    Time.May -> "May"
    Time.Jun -> "June"
    Time.Jul -> "July"
    Time.Aug -> "August"
    Time.Sep -> "September"
    Time.Oct -> "October"
    Time.Nov -> "November"
    Time.Dec -> "December"


weekdayToString : Time.Weekday -> String
weekdayToString w =
  case w of
    Time.Mon -> "Monday"
    Time.Tue -> "Tuesday"
    Time.Wed -> "Wednesday"
    Time.Thu -> "Thursday"
    Time.Fri -> "Friday"
    Time.Sat -> "Saturday"
    Time.Sun -> "Sunday"


type alias Model = {
  today: Date
  , year: Maybe Year
  }

getCalendarDate : Task.Task x Date
getCalendarDate =
  Task.map2 toCalendarDate Time.here Time.now

toCalendarDate : Time.Zone -> Time.Posix -> Date
toCalendarDate zone now =
  let
      rawDate = 
        { day = (Time.toDay zone now)
        , month = (Time.toMonth zone now)
        , year = (Time.toYear zone now)
        }
  in
      (Calendar.fromRawParts rawDate) |> Maybe.withDefault nullDate

init : () -> (Model, Cmd Msg)
init _ =
  (
  { today = nullDate
  , year = Nothing
  }, Task.perform SetToday getCalendarDate
  )


-- UPDATE

type Msg = SetToday Date

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetToday today ->
          ( { model | today = today, year = Just (yearFromDate today) }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

dateToString : Date -> String
dateToString d =
  weekdayToString (Calendar.getWeekday d)
  ++ ", " ++
  String.fromInt (Calendar.getYear d)
  ++ "-" ++
  monthToString (Calendar.getMonth d)
  ++ "-" ++
  String.fromInt (Calendar.getDay d)



view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (dateToString model.today) ]
    , case model.year of
      Just year -> yearView year
      Nothing ->
        text "no year"
    ]

yearView : Year -> Html Msg
yearView year =
  Html.ol [] (List.map yearDayView year.days)

yearDayView : Day -> Html Msg
yearDayView day =
  Html.li [
    classList 
      [ ("day-first-of-month", day.firstOfMonth)
      , ("day-weekend", day.isWeekend)
      ]
    ]
    [ text (if day.firstOfMonth then "F" else "")
    , text (if day.isWeekend then "W" else "")
    , text " "
    , text (dateToString day.date)
    , text " "
    , text (String.fromInt (day.index+1))
    , text " / "
    , text (String.fromInt (day.daysInYear))
    -- , text (String.fromBool day.isWeekend)
    ]

