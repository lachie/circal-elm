module CalendarEvent exposing (EventDetail(..), EventTime(..), Evt(..), Location(..), Repetition(..), view)

import Calendar
import Color
import Date
import Debug exposing (log)
import Time exposing (Month(..))
import TypedSvg exposing (circle, g, line, path, svg, text_, title)
import TypedSvg.Attributes exposing (class, cx, cy, d, fill, fontSize, height, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), px)
import ViewHelpers exposing (ViewFacts, YearDirection(..), arcLabel)


type Location
    = Airport String
    | City String


type alias SimpleTime =
    Int


type EventTime
    = LocatedTime Int Month Int SimpleTime Location
    | EventDate Int Month Int
    | EventTime Int Month Int SimpleTime


type Evt
    = Events (List EventDetail) (List Evt)
    | RangeEvent EventTime EventTime (List EventDetail)
    | Event EventTime (List EventDetail)
    | RepeatEvent Repetition (List Evt) (List EventDetail)


type EventDetail
    = Flight String
    | Accom String
    | Pending
    | Birthday String
    | Desc String


type alias EventViewFacts =
    { dateAngle : EventTime -> Float
    , viewFacts : ViewFacts

    --, eventRadius : Float -> Float
    }


eventDateJDN : EventTime -> Int
eventDateJDN d =
    case d of
        LocatedTime year month day _ _ ->
            dateJDN year month day

        EventDate year month day ->
            dateJDN year month day

        EventTime year month day _ ->
            dateJDN year month day


dateJDN : Int -> Month -> Int -> Int
dateJDN year month day =
    Date.julianDayNumber year (Date.monthOrd month) day


view : ViewFacts -> List Evt -> Svg msg
view facts events =
    let
        _ =
            log "events" events

        dateAngle y m d =
            dateJDN y m d |> facts.dayAngleJDN

        _ =
            log "fdJDN" facts.startJDN

        evtFacts =
            { dateAngle = eventDateJDN >> facts.dayAngleJDN
            , viewFacts = facts

            --, eventRadius = facts.dayRadiusN
            }
    in
    g []
        (List.map
            (eventView evtFacts)
            events
        )


eventView : EventViewFacts -> Evt -> Svg msg
eventView facts evt =
    case evt of
        Events details subEvents ->
            groupedEventView facts details subEvents

        RangeEvent startTime endTime details ->
            rangeEventView facts details startTime endTime

        Event date details ->
            singletonEventView facts details date

        RepeatEvent repetition seedEvents details ->
            repeatedEventView facts repetition seedEvents details


groupedEventView : EventViewFacts -> List EventDetail -> List Evt -> Svg msg
groupedEventView facts details events =
    g []
        (List.map
            (\i -> eventView facts i)
            events
        )


rangeEventView : EventViewFacts -> List EventDetail -> EventTime -> EventTime -> Svg msg
rangeEventView facts details startTime endTime =
    let
        startAngle =
            facts.dateAngle startTime

        endAngle =
            facts.dateAngle endTime
    in
    path
        [ d (arcPath startAngle endAngle (facts.viewFacts.daysRadiusN 1))
        , strokeWidth (px 5)
        , stroke <| Color.blue
        ]
        [ title [] [ text "ranged" ]
        ]


singletonEventView : EventViewFacts -> List EventDetail -> EventTime -> Svg msg
singletonEventView facts details date =
    circle
        [ cx (px 0)
        , cy (px 0)
        , fill <| Fill Color.yellow
        , r (px (facts.viewFacts.dayRadiusN 1))
        , transform [ Rotate (facts.dateAngle date) 0 0, Translate (facts.viewFacts.daysRadiusN 1) 0 ]
        ]
        [ title [] [ text "singelton" ]
        ]


type Repetition
    = EveryNWeeks Int
    | EveryNYears Int
    | EveryYear


repeatedEventView : EventViewFacts -> Repetition -> List Evt -> List EventDetail -> Svg msg
repeatedEventView facts repetition seedEvents details =
    repeatEvents repetition seedEvents |> groupedEventView facts details


repeatEvents : Repetition -> List Evt -> List Evt
repeatEvents repetition seedEvents =
    case repetition of
        EveryNWeeks n ->
            repeatWeeks n seedEvents

        EveryNYears n ->
            repeatYears n seedEvents

        EveryYear ->
            repeatEvents (EveryNYears 1) seedEvents


repeatWeeks : Int -> List Evt -> List Evt
repeatWeeks n seedEvents =
    []


repeatYears : Int -> List Evt -> List Evt
repeatYears n seedEvents =
    []



-- g [] []


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



-- title [] [ text "accom" ]
-- ]
-- circle
-- [ cx (px 0)
-- , cy (px 0)
-- , r (px (facts.eventRadius facts.radius))
-- , transform [ Rotate (facts.tripTimeToAngle time) 0 0, Translate (facts.radius * 1.01) 0 ]
-- ]
-- []
-- flightView : EventViewFacts -> Desc -> TripTime -> TripTime -> String -> Svg msg
-- flightView facts desc startTime endTime transport =
-- let
-- flightDesc =
-- startTime.loc ++ "->" ++ endTime.loc
-- in
-- g []
-- [ circle
-- [ cx (px 0)
-- , cy (px 0)
-- , fill <| Fill Color.green
-- , r (px (facts.eventRadius facts.radius))
-- , transform [ Rotate (facts.tripTimeToAngle startTime) 0 0, Translate (facts.radius * 1) 0 ]
-- ]
-- [ title [] [ text ("flight start " ++ desc ++ " " ++ flightDesc) ]
-- ]
-- , circle
-- [ cx (px 0)
-- , cy (px 0)
-- , fill <| Fill Color.red
-- , r (px (facts.eventRadius facts.radius))
-- , transform [ Rotate (facts.tripTimeToAngle endTime) 0 0, Translate (facts.radius * 0.98) 0 ]
-- ]
-- [ title [] [ text ("flight end " ++ desc ++ " " ++ flightDesc) ]
-- ]
-- ]
