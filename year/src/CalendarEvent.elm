module CalendarEvent exposing (CalendarEvent, Event, EventDetails(..), TripTime, view)

import Calendar
import Color
import Date
import Debug exposing (log)
import Time exposing (Month(..))
import TypedSvg exposing (circle, g, line, path, svg, text_, title)
import TypedSvg.Attributes exposing (class, cx, cy, d, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import ViewHelpers exposing (YearDirection(..), YearViewFacts, arcLabel)


type alias Location =
    String


type alias Desc =
    String


type alias TripTime =
    { year : Int
    , month : Month
    , day : Int
    , time : Int
    , loc : Location
    }


type alias Event =
    ( Desc, EventDetails )


type EventDetails
    = Trip (List Event)
    | Accomodation TripTime TripTime String
    | Flight TripTime TripTime String
    | Group (List EventDetails)
    | Birthday String Int Month Int


type alias CalendarEvent =
    { date : Calendar.Date
    , desc : String
    }


type alias EventViewFacts =
    { tripTimeToAngle : TripTime -> Float
    , dateAngle : Int -> Month -> Int -> Float
    , radius : Float
    , eventRadius : Float -> Float
    }


dateToTripTime : Calendar.Date -> TripTime
dateToTripTime d =
    { year = Calendar.getYear d
    , month = Calendar.getMonth d
    , day = Calendar.getDay d
    , time = 0
    , loc = ""
    }


tripTimeJDN : TripTime -> Int
tripTimeJDN { year, month, day } =
    Date.julianDayNumber year (Date.monthOrd month) day


dateJDN : Int -> Month -> Int -> Int
dateJDN year month day =
    Date.julianDayNumber year (Date.monthOrd month) day


view : YearViewFacts -> List Event -> Svg msg
view facts events =
    let
        _ =
            log "events" events

        _ =
            log "dr" (facts.dayRadius facts.daysRadius)

        firstDayJDN =
            Date.toJulianDayNumber facts.firstDate

        deltaJDN =
            \x -> x - firstDayJDN

        _ =
            log "fdJDN" firstDayJDN

        evtFacts =
            { tripTimeToAngle = tripTimeJDN >> deltaJDN >> facts.dayAngle
            , dateAngle = dateJDN >> deltaJDN >> facts.dayAngle
            , radius = facts.daysRadius -- - facts.dayRadius facts.daysRadius * 3
            , eventRadius = facts.dayRadius
            }
    in
    g []
        (List.map
            (eventView evtFacts)
            events
        )


eventView : EventViewFacts -> Event -> Svg msg
eventView facts ( desc, details ) =
    case details of
        Trip legs ->
            tripView facts desc legs

        Accomodation startTime endTime name ->
            accomView facts desc startTime endTime name

        Flight startTime endTime transport ->
            flightView facts desc startTime endTime transport

        Birthday name year month day ->
            birthdayView facts name year month day

        Group items ->
            g [] List.map (\i -> eventView ( "", i )) items


tripView : EventViewFacts -> Desc -> List Event -> Svg msg
tripView facts desc legs =
    g [] (List.map (eventView facts) legs)


arcPath : Float -> Float -> Float -> String
arcPath a1deg a2deg r =
    let
        a1 =
            degrees a1deg

        a2 =
            degrees a2deg

        x1 =
            r * cos a1

        y1 =
            r * sin a1

        x2 =
            r * cos a2

        y2 =
            r * sin a2

        si =
            String.fromFloat
    in
    "M " ++ si x1 ++ "," ++ si y1 ++ " A" ++ si r ++ "," ++ si r ++ " 0 0,0 " ++ si x2 ++ "," ++ si y2


accomView : EventViewFacts -> Desc -> TripTime -> TripTime -> String -> Svg msg
accomView facts desc startTime endTime name =
    let
        startAngle =
            facts.tripTimeToAngle startTime

        endAngle =
            facts.tripTimeToAngle endTime
    in
    path
        [ d (arcPath startAngle endAngle (facts.radius * 1.01))
        , strokeWidth (px 5)
        , stroke <| Color.blue
        ]
        [ title [] [ text ("accom " ++ desc ++ " " ++ name) ]
        ]



-- title [] [ text "accom" ]
-- ]
-- circle
-- [ cx (px 0)
-- , cy (px 0)
-- , r (px (facts.eventRadius facts.radius))
-- , transform [ Rotate (facts.tripTimeToAngle time) 0 0, Translate (facts.radius * 1.01) 0 ]
-- ]
-- []


flightView : EventViewFacts -> Desc -> TripTime -> TripTime -> String -> Svg msg
flightView facts desc startTime endTime transport =
    let
        flightDesc =
            startTime.loc ++ "->" ++ endTime.loc
    in
    g []
        [ circle
            [ cx (px 0)
            , cy (px 0)
            , fill <| Fill Color.green
            , r (px (facts.eventRadius facts.radius))
            , transform [ Rotate (facts.tripTimeToAngle startTime) 0 0, Translate (facts.radius * 1) 0 ]
            ]
            [ title [] [ text ("flight start " ++ desc ++ " " ++ flightDesc) ]
            ]
        , circle
            [ cx (px 0)
            , cy (px 0)
            , fill <| Fill Color.red
            , r (px (facts.eventRadius facts.radius))
            , transform [ Rotate (facts.tripTimeToAngle endTime) 0 0, Translate (facts.radius * 0.98) 0 ]
            ]
            [ title [] [ text ("flight end " ++ desc ++ " " ++ flightDesc) ]
            ]
        ]


birthdayView : EventViewFacts -> String -> Int -> Month -> Int -> Svg msg
birthdayView facts name year month day =
    circle
        [ cx (px 0)
        , cy (px 0)
        , fill <| Fill Color.yellow
        , r (px (facts.eventRadius facts.radius))
        , transform [ Rotate (facts.dateAngle year month day) 0 0, Translate (facts.radius * 1) 0 ]
        ]
        [ title [] [ text name ]
        ]
