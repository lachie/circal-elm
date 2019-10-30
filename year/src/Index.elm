module Main exposing (main)

import Browser
import Calendar
import CalendarEvent exposing (EventDetail(..), EventTime(..), Evt(..), Location(..), Repetition(..))
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
    , events : List Evt
    }


events =
    [ Events [ Desc "Accelerate/New Orleans" ]
        [ RangeEvent (LocatedTime 2019 May 17 1740 (Airport "SYD")) (LocatedTime 2019 May 17 1400 (Airport "SFO")) [ Flight "QF73" ]
        , RangeEvent (LocatedTime 2019 May 17 0 (City "SF")) (LocatedTime 2019 May 27 0 (City "SF")) [ Accom "Hotel Emblem" ]
        , RangeEvent (LocatedTime 2019 May 27 1300 (Airport "SFO")) (LocatedTime 2019 May 27 1910 (Airport "MSY")) [ Flight "AS1390" ]
        , RangeEvent (LocatedTime 2019 May 27 0 (City "NOLA")) (LocatedTime 2019 Jun 3 0 (City "NOLA")) [ Accom "Airbnb" ]
        , RangeEvent (LocatedTime 2019 Jun 3 2030 (Airport "MSY")) (LocatedTime 2019 Jun 3 2310 (Airport "SFO")) [ Accom "AS1391" ]
        , RangeEvent (LocatedTime 2019 Jun 3 0 (City "SF")) (LocatedTime 2019 Jun 4 0 (City "SF")) [ Pending ]
        , RangeEvent (LocatedTime 2019 Jun 4 2225 (Airport "SFO")) (LocatedTime 2019 Jun 6 605 (Airport "SYD")) [ Flight "QF74" ]
        ]
    , Events [ Desc "Vietnam Offsite" ]
        [ RangeEvent (LocatedTime 2019 Oct 20 1115 (Airport "SYD")) (LocatedTime 2019 Oct 20 1610 (Airport "SGN")) [ Flight "VN772" ]
        , RangeEvent (LocatedTime 2019 Oct 20 0 (City "Saigon")) (LocatedTime 2019 Oct 27 0 (City "Saigon")) [ Accom "Intercontinental" ]
        , RangeEvent (LocatedTime 2019 Oct 26 2045 (Airport "SGN")) (LocatedTime 2019 Oct 27 915 (Airport "SYD")) [ Flight "VN773" ]
        ]
    , Events [ Desc "Melbourne / Railscamp" ]
        [ RangeEvent (LocatedTime 2019 Nov 9 1700 (Airport "SYD")) (LocatedTime 2019 Nov 9 1835 (Airport "MEL")) [ Flight "QF453" ]
        , RangeEvent (LocatedTime 2019 Nov 9 0 (City "Melbourne")) (LocatedTime 2019 Nov 15 0 (City "Melbourne")) [ Accom "Airbnb" ]
        , RangeEvent (LocatedTime 2019 Nov 15 0 (City "Kyneton")) (LocatedTime 2019 Nov 18 0 (City "Kyneton")) [ Accom "RailsCamp" ]
        , RangeEvent (LocatedTime 2019 Nov 18 1830 (Airport "MEL")) (LocatedTime 2019 Nov 18 1955 (Airport "SYD")) [ Flight "QF460" ]
        ]
    , RepeatEvent EveryYear
        [ Event (EventDate 2006 Nov 9) [ Birthday "Toby" ]
        , Event (EventDate 2009 Jun 25) [ Birthday "Sam" ]
        ]
        [ Desc "Birthdays" ]

    -- , Events [ Desc "Routine" ]
    -- [ RepeatEvent (EveryNWeeks 4)
    -- [ RangeEvent (EventDate 2019 Apr 20) (EventDate 2019 May 4) [ Desc "Ilona Kids" ]
    -- , RangeEvent (EventDate 2019 May 4) (EventDate 2019 May 18) [ Desc "Lachie Kids" ]
    -- ]
    -- []
    -- ]
    ]



-- events =
-- tree RootEvent
-- [ tree (Events [ Desc "Vietnam Offsite" ])
-- [ singleton (RangeEvent (LocatedTime 2019 Oct 20 1115 (Airport "SYD")) (LocatedTime 2019 Oct 20 1610 (Airport "SGN")) [ Flight "VN772" ])
-- , singleton (RangeEvent (LocatedTime 2019 Oct 20 0 (City "Saigon")) (LocatedTime 2019 Oct 27 0 (City "Saigon")) [ Accom "Intercontinental" ])
-- , singleton (RangeEvent (LocatedTime 2019 Oct 26 2045 (Airport "SGN")) (LocatedTime 2019 Oct 27 915 (Airport "SYD")) [ Flight "VN773" ])
-- ]
-- , tree (Events [ Desc "Melbourne/RailsCamp" ])
-- [ singleton (RangeEvent (LocatedTime 2019 Oct 20 1115 (Airport "SYD")) (LocatedTime 2019 Oct 20 1610 (Airport "SGN")) [ Flight "VN772" ])
-- ]
-- , tree (RepeatEvent EveryYear [ Desc "Birthdays" ])
-- [ singleton (Event (EventDate 2006 Nov 9) [ Birthday "Toby" ])
-- , singleton (Event (EventDate 2009 Jun 25) [ Birthday "Sam" ])
-- ]
-- , tree (Events [ Desc "Routine" ])
-- [ tree (RepeatEvent (EveryNWeeks 4))
-- [ singleton RangeEvent (EventDate 2019 Apr 20) (EventDate 2019 May 4) [ Desc "Ilona Kids" ]
-- , singleton RangeEvent (EventDate 2019 May 4) (EventDate 2019 May 18) [ Desc "Lachie Kids" ]
-- ]
-- ]
-- ]


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
                Year.view 600 year model.events

            Nothing ->
                Html.text "no year"
        ]
