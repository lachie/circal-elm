module CalendarEvent exposing (EventDetail(..), EventTime(..), Evt(..), Location(..), Repetition(..), view)

import Calendar
import Color
import Date
import Day exposing (Day)
import Debug exposing (log)
import Time exposing (Month(..))
import Tree exposing (Tree, singleton, tree)
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


type RealEvent
    = SimpleEvent SimpleEventData
    | GroupEvent String
    | NotImpl
    | Filtered
    | Root


type alias YearFacts =
    { jdnInYear : Int -> Bool
    , startJDN : Int
    , endJDN : Int
    , year1 : Int
    , year2 : Int
    }


type alias SimpleEventData =
    { start : Int
    , end : Int
    , label : String
    }


reifyEvents : YearFacts -> List Evt -> Tree RealEvent
reifyEvents year events =
    tree Root (List.map (reifyEvent year) events)


reifyEvent : YearFacts -> Evt -> Tree RealEvent
reifyEvent year evt =
    case evt of
        Events details subEvents ->
            reifyGroupedEvent year details subEvents

        RangeEvent startTime endTime details ->
            reifyRangeEvent year details startTime endTime

        Event date details ->
            reifySingleEvent year details date

        RepeatEvent repetition seedEvents details ->
            reifyRepeatedEvent year repetition details seedEvents


evtDetailDescs : List EventDetail -> String
evtDetailDescs details =
    List.map eventDetailToString details |> String.join ", "


reifyGroupedEvent : YearFacts -> List EventDetail -> List Evt -> Tree RealEvent
reifyGroupedEvent year details events =
    let
        desc =
            evtDetailDescs details
    in
    tree (GroupEvent desc)
        (List.map
            (reifyEvent year)
            events
        )


reifyRangeEvent : YearFacts -> List EventDetail -> EventTime -> EventTime -> Tree RealEvent
reifyRangeEvent year details startTime endTime =
    let
        clamped =
            clampJDNRange year (eventDateJDN startTime) (eventDateJDN endTime)
    in
    case clamped of
        Just ( start, end ) ->
            singleton (SimpleEvent (SimpleEventData start end (evtDetailDescs details)))

        Nothing ->
            singleton Filtered


reifySingleEvent : YearFacts -> List EventDetail -> EventTime -> Tree RealEvent
reifySingleEvent year details date =
    let
        jdn =
            eventDateJDN date
    in
    if year.jdnInYear jdn then
        singleton (SimpleEvent (SimpleEventData jdn jdn (evtDetailDescs details)))

    else
        singleton Filtered


reifyRepeatedEvent : YearFacts -> Repetition -> List EventDetail -> List Evt -> Tree RealEvent
reifyRepeatedEvent year repetition groupDetails seedEvents =
    case repetition of
        EveryNWeeks n ->
            repeatWeeks year n groupDetails seedEvents

        EveryNYears n ->
            repeatYears year n groupDetails seedEvents

        EveryYear ->
            reifyRepeatedEvent year (EveryNYears 1) groupDetails seedEvents


repeatWeeks : YearFacts -> Int -> List EventDetail -> List Evt -> Tree RealEvent
repeatWeeks year n groupDetails seedEvents =
    singleton Filtered


repeatYears : YearFacts -> Int -> List EventDetail -> List Evt -> Tree RealEvent
repeatYears year n groupDetails seedEvents =
    tree (GroupEvent ("repeat years " ++ String.fromInt n))
        (List.map
            (repeatYears_ year n groupDetails)
            seedEvents
        )


repeatYears_ : YearFacts -> Int -> List EventDetail -> Evt -> Tree RealEvent
repeatYears_ year stride groupDetails seedEvent =
    let
        nextYear =
            year.year2

        -- eventDateJDN seedEvent
        added =
            Date.addYears year.startJDN 1

        _ =
            Debug.log "seed" seedEvent
    in
    case seedEvent of
        Events details subEvents ->
            singleton Filtered

        RangeEvent startTime endTime details ->
            -- TODO adjust start/end
            -- TODO merge details
            reifyRangeEvent year (List.concat [ details, groupDetails ]) startTime endTime

        Event date details ->
            let
                adjustedDate =
                    adjustYear year stride date
            in
            case adjustedDate of
                Just newDate ->
                    reifySingleEvent year (List.concat [ details, groupDetails ]) newDate

                Nothing ->
                    singleton Filtered

        -- prune nested repeated
        RepeatEvent repetition seedEvents details ->
            singleton Filtered


adjustYear : YearFacts -> Int -> EventTime -> Maybe EventTime
adjustYear year stride evtDate =
    let
        _ =
            Debug.log "year" year

        nextYear =
            year.year2

        originJDN =
            eventDateJDN evtDate

        ( originYear, _, _ ) =
            Date.fromJulianDayNumber originJDN

        years =
            rangeStride stride originYear nextYear |> List.filter (\x -> x >= year.year1) |> List.reverse

        _ =
            Debug.log "a" years

        delta =
            \y -> y - originYear

        addYears =
            Date.addYears originJDN

        -- filter = delta >> addYears >> year.jdnInYear
        matchingDate =
            List.map (delta >> addYears) years |> List.filter year.jdnInYear |> List.head
    in
    Maybe.map (setDateJDN evtDate) matchingDate


setYear : Int -> EventTime -> EventTime
setYear year date =
    case date of
        EventDate _ m d ->
            EventDate year m d

        LocatedTime _ m d time location ->
            LocatedTime year m d time location

        EventTime _ m d time ->
            EventTime year m d time


setDateJDN : EventTime -> Int -> EventTime
setDateJDN evtDate newJDN =
    let
        ( newYear, newMonth, newDay ) =
            Date.fromJulianDayNumber newJDN
    in
    case evtDate of
        EventDate _ _ _ ->
            EventDate newYear newMonth newDay

        LocatedTime _ _ _ time location ->
            LocatedTime newYear newMonth newDay time location

        EventTime _ _ _ time ->
            EventTime newYear newMonth newDay time


rangeStride : Int -> Int -> Int -> List Int
rangeStride stride from to =
    if to < from then
        []

    else
        from :: rangeStride stride (from + stride) to


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


eventDetailToString : EventDetail -> String
eventDetailToString detail =
    case detail of
        Birthday name ->
            name ++ "'s birthday"

        Flight f ->
            f

        Accom a ->
            a

        Pending ->
            "pending"

        Desc d ->
            d


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


clampJDNRange : YearFacts -> Int -> Int -> Maybe ( Int, Int )
clampJDNRange year start end =
    let
        _ =
            Debug.log "year" year

        _ =
            Debug.log "start" start

        _ =
            Debug.log "end" end

        startInRange =
            year.jdnInYear start

        _ =
            Debug.log "startInRange" startInRange

        endInRange =
            year.jdnInYear end

        _ =
            Debug.log "endInRange" endInRange
    in
    if not startInRange && not endInRange then
        Nothing

    else
        Just ( clampJDN year start, clampJDN year end )


clampJDN : YearFacts -> Int -> Int
clampJDN year jdn =
    if jdn < year.startJDN then
        year.startJDN

    else if year.endJDN < jdn then
        year.endJDN

    else
        jdn


dateJDN : Int -> Month -> Int -> Int
dateJDN year month day =
    Date.julianDayNumber year (Date.monthOrd month) day


view : ViewFacts -> List Evt -> Svg msg
view facts events =
    let
        _ =
            log "events" events

        ( year1, _, _ ) =
            Date.fromJulianDayNumber facts.startJDN

        ( year2, _, _ ) =
            Date.fromJulianDayNumber facts.endJDN

        reifiedEvents =
            reifyEvents
                { jdnInYear = facts.jdnInYear
                , startJDN = facts.startJDN
                , endJDN = facts.endJDN
                , year1 = year1
                , year2 = year2
                }
                events

        _ =
            log "reifiedEvents" reifiedEvents

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
    reifiedEvents
        |> Tree.restructure (labelToSVG evtFacts) (toListItems evtFacts)
        |> (\root -> g [] [ root ])


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
        [ d (arcPath startAngle endAngle (facts.viewFacts.radiusN 1))
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
        , transform [ Rotate (facts.dateAngle date) 0 0, Translate (facts.viewFacts.radiusN 1) 0 ]
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
            repeatWeeks__ n seedEvents

        EveryNYears n ->
            repeatYears__ n seedEvents

        EveryYear ->
            repeatEvents (EveryNYears 1) seedEvents


repeatWeeks__ : Int -> List Evt -> List Evt
repeatWeeks__ n seedEvents =
    []


repeatYears__ : Int -> List Evt -> List Evt
repeatYears__ n seedEvents =
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
