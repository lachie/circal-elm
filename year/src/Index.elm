module Main exposing (main)

import Browser
import Calendar
import CalendarEvent exposing (CalendarEvent, Event, EventDetails(..), TripTime)
import Color
import Date
import Debug exposing (log)
import Html exposing (Html, button, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Month
import Task
import Time exposing (Month(..))
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import Year


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL
-- Year
-- Day


type alias Model =
    { today : Calendar.Date
    , year : Maybe Year.Year
    , events : List Event
    }


events =
    [ ( "Accelerate/New Orleans"
      , Trip
            [ ( "", Flight (TripTime 2019 May 17 1740 "SYD") (TripTime 2019 May 17 1400 "SFO") "QF73" )
            , ( "", Accomodation (TripTime 2019 May 17 0 "SF") (TripTime 2019 May 27 0 "SF") "Hotel Emblem" )
            , ( "", Flight (TripTime 2019 May 27 1300 "SFO") (TripTime 2019 May 27 1910 "MSY") "AS1390" )
            , ( "", Accomodation (TripTime 2019 May 27 0 "NOLA") (TripTime 2019 Jun 3 0 "NOLA") "Airbnb" )
            , ( "", Flight (TripTime 2019 Jun 3 2030 "MSY") (TripTime 2019 Jun 3 2310 "SFO") "AS1391" )
            , ( "", Flight (TripTime 2019 Jun 4 2225 "SFO") (TripTime 2019 Jun 6 605 "SYD") "QF74" )
            ]
      )
    , ( "Birthday"
      , Birthdays
            [ ( "", Birthday "Toby" 2006 Nov 9 )
            ]
      )
    ]


getCalendarDate : Task.Task x Calendar.Date
getCalendarDate =
    Task.map2 toCalendarDate Time.here Time.now


toCalendarDate : Time.Zone -> Time.Posix -> Calendar.Date
toCalendarDate zone now =
    let
        rawDate =
            { day = Time.toDay zone now
            , month = Time.toMonth zone now
            , year = Time.toYear zone now
            }
    in
    Calendar.fromRawParts rawDate |> Maybe.withDefault Date.nullDate


init : () -> ( Model, Cmd Msg )
init _ =
    ( { today = Date.nullDate
      , year = Nothing
      , events = events
      }
    , Task.perform SetToday getCalendarDate
    )



-- UPDATE


type Msg
    = SetToday Calendar.Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetToday today ->
            ( { model | today = today, year = Just (Year.fromDate today) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.text (Date.toString model.today) ]
        , case model.year of
            Just year ->
                Year.view 450 year model.events

            Nothing ->
                Html.text "no year"
        ]
