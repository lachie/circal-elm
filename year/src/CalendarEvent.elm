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


type alias YearFacts =
    { jdnInYear : Int -> Bool
    , startJDN : Int
    , endJDN : Int
    , year1 : Int
    , year2 : Int
    }


type alias EventLabel =
    { startJDN : Int
    , endJDN : Int
    , label : String
    , depth : Int
    , width : Int
    , height : Int
    , omit : Bool
    }


newEventLabel : Int -> Int -> String -> EventLabel
newEventLabel startJDN endJDN label =
    { startJDN = startJDN
    , endJDN = endJDN
    , label = label
    , depth = 0
    , width = endJDN - startJDN
    , height = 1
    , omit = False
    }


nullEventLabel : EventLabel
nullEventLabel =
    { startJDN = 0
    , endJDN = 0
    , label = "null"
    , depth = 0
    , width = 0
    , height = 0
    , omit = False
    }


omittedEvent : EventLabel
omittedEvent =
    { nullEventLabel | omit = True }


reifyEvents : YearFacts -> List Evt -> Tree EventLabel
reifyEvents year events =
    tree { nullEventLabel | label = "root" } (List.map (reifyEvent year) events)



--|> List.filter (Tree.label >> .omit >> not)


reifyEvent : YearFacts -> Evt -> Tree EventLabel
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


reifyGroupedEvent : YearFacts -> List EventDetail -> List Evt -> Tree EventLabel
reifyGroupedEvent year details events =
    let
        desc =
            evtDetailDescs details
    in
    Tree.tree { nullEventLabel | label = desc }
        (List.map
            (reifyEvent year)
            events
        )


reifyRangeEvent : YearFacts -> List EventDetail -> EventTime -> EventTime -> Tree EventLabel
reifyRangeEvent year details startTime endTime =
    let
        clamped =
            clampJDNRange year (eventDateJDN startTime) (eventDateJDN endTime)
    in
    case clamped of
        Just ( start, end ) ->
            singleton (newEventLabel start end (evtDetailDescs details))

        Nothing ->
            singleton omittedEvent


reifySingleEvent : YearFacts -> List EventDetail -> EventTime -> Tree EventLabel
reifySingleEvent year details date =
    let
        jdn =
            eventDateJDN date
    in
    if year.jdnInYear jdn then
        singleton (newEventLabel jdn jdn (evtDetailDescs details))

    else
        singleton omittedEvent


reifyRepeatedEvent : YearFacts -> Repetition -> List EventDetail -> List Evt -> Tree EventLabel
reifyRepeatedEvent year repetition groupDetails seedEvents =
    case repetition of
        EveryNWeeks n ->
            repeatWeeks year n groupDetails seedEvents

        EveryNYears n ->
            repeatYears year n groupDetails seedEvents

        EveryYear ->
            reifyRepeatedEvent year (EveryNYears 1) groupDetails seedEvents


repeatWeeks : YearFacts -> Int -> List EventDetail -> List Evt -> Tree EventLabel
repeatWeeks year n groupDetails seedEvents =
    singleton omittedEvent


repeatYears : YearFacts -> Int -> List EventDetail -> List Evt -> Tree EventLabel
repeatYears year n groupDetails seedEvents =
    Tree.tree { nullEventLabel | label = "repeat years " ++ String.fromInt n }
        (List.map
            (repeatYears_ year n groupDetails)
            seedEvents
        )


repeatYears_ : YearFacts -> Int -> List EventDetail -> Evt -> Tree EventLabel
repeatYears_ year stride groupDetails seedEvent =
    let
        nextYear =
            year.year2

        -- eventDateJDN seedEvent
        added =
            Date.addYears year.startJDN 1
    in
    case seedEvent of
        Events details subEvents ->
            singleton omittedEvent

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
                    singleton omittedEvent

        -- prune nested repeated
        RepeatEvent repetition seedEvents details ->
            singleton omittedEvent


adjustYear : YearFacts -> Int -> EventTime -> Maybe EventTime
adjustYear year stride evtDate =
    let
        nextYear =
            year.year2

        originJDN =
            eventDateJDN evtDate

        ( originYear, _, _ ) =
            Date.fromJulianDayNumber originJDN

        years =
            rangeStride stride originYear nextYear |> List.filter (\x -> x >= year.year1) |> List.reverse

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
    , jdnAngle : Int -> Float
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
        startInRange =
            year.jdnInYear start

        endInRange =
            year.jdnInYear end
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


isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        _ ->
            False


isJust =
    isNothing >> not


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

        laidOutEvents =
            layoutEvents 0 reifiedEvents

        _ =
            log "laidOutEvents" laidOutEvents

        dateAngle y m d =
            dateJDN y m d |> facts.dayAngleJDN

        _ =
            log "fdJDN" facts.startJDN

        evtFacts =
            { dateAngle = eventDateJDN >> facts.dayAngleJDN
            , jdnAngle = facts.dayAngleJDN
            , viewFacts = facts

            --, eventRadius = facts.dayRadiusN
            }
    in
    evtView evtFacts 0 laidOutEvents


setDim : EventLabel -> Int -> EventLabel
setDim label depth =
    { label | depth = depth }


layoutEvents : Int -> Tree EventLabel -> Tree EventLabel
layoutEvents depth t =
    let
        label =
            Tree.label t

        children =
            Tree.children t
    in
    case children of
        [] ->
            Tree.singleton (setDim label depth)

        _ ->
            let
                laidOutChildren =
                    List.map (layoutEvents (depth + 1)) children

                laidOutLabel =
                    layoutLabel label laidOutChildren
            in
            Tree.tree laidOutLabel laidOutChildren


type alias LayoutAcc =
    { depth : Int
    , done : List EventLabel
    }


layoutLabel : EventLabel -> List (Tree EventLabel) -> EventLabel
layoutLabel label children =
    nullEventLabel


type alias Dim =
    { width : Int, height : Int }


foldLabelDim : Tree EventLabel -> Dim -> Dim
foldLabelDim t dim =
    let
        evt =
            Tree.label t
    in
    let
        w =
            evt.endJDN - evt.startJDN + 1
    in
    { width = max w dim.width, height = max evt.height dim.height }


layoutLabel_ : EventLabel -> List (Tree EventLabel) -> EventLabel
layoutLabel_ label children =
    let
        dim =
            List.foldl foldLabelDim { width = 0, height = 0 } children
    in
    label



-- case label of
-- SimpleEvent evtData -> { evtData
-- { label


evtView : EventViewFacts -> Int -> Tree EventLabel -> Svg msg
evtView facts depth t =
    let
        evtData =
            Tree.label t

        children =
            Tree.children t
    in
    case children of
        [] ->
            singletonEvtView facts depth evtData

        _ ->
            g []
                (groupEvtView facts depth evtData
                    :: List.map (evtView facts (depth + 1)) children
                )


groupEvtView : EventViewFacts -> Int -> EventLabel -> Svg msg
groupEvtView facts depth evt =
    let
        _ =
            Debug.log "groupEvtView" evt
    in
    g [] []


singletonEvtView : EventViewFacts -> Int -> EventLabel -> Svg msg
singletonEvtView facts depth evt =
    let
        startAngle =
            facts.jdnAngle evt.startJDN

        endAngle =
            facts.jdnAngle evt.endJDN

        _ =
            Debug.log "startAngle" startAngle

        _ =
            Debug.log "endAngle" endAngle
    in
    if startAngle == endAngle then
        circle
            [ cx (px 0)
            , cy (px 0)
            , transform [ Rotate startAngle 0 0, Translate (facts.viewFacts.radiusN 1) 0 ]
            , fill <| Fill Color.blue
            , r (px (facts.viewFacts.dayRadiusN 1))
            ]
            [ title [] [ text evt.label ]
            ]

    else
        path
            [ d (arcPath startAngle endAngle (facts.viewFacts.radiusN depth))
            , strokeWidth (px 5)
            , stroke <| Color.blue
            ]
            [ title [] [ text evt.label ]
            ]



-- layout


unboxMaybes : List (Maybe a) -> List a
unboxMaybes l =
    case l of
        head :: rest ->
            case head of
                Just x ->
                    x :: unboxMaybes rest

                _ ->
                    unboxMaybes rest

        _ ->
            []


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


collapseTree : Tree EventLabel -> Maybe (Tree EventLabel)
collapseTree t =
    let
        label =
            Tree.label t
    in
    if label.omit then
        Nothing

    else
        let
            originalChildren =
                Tree.children t
        in
        case originalChildren of
            [] ->
                Just (Tree.singleton label)

            _ ->
                let
                    filteredChildren =
                        originalChildren |> List.filter (Tree.label >> .omit >> not)
                in
                case filteredChildren of
                    [] ->
                        Nothing

                    _ ->
                        Just (Tree.tree label filteredChildren)
