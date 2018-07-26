module View.Year exposing (..)

import Html exposing (..)
import Html.Attributes as HAttrs
import Color
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (Fill(..), Transform(..), ShapeRendering(..), px)
import Svg.Path exposing (pathToString, subpath, startAt, lineToMany, lineTo, emptySubpath, closed, arcTo, smallestArc, clockwise, antiClockwise)
import View.Mouse exposing (onMouseMove, Point)
import Types.Year exposing (..)


type Msg
    = CurrentDate Date


type alias YearViewState =
    { angle : Float
    , dayIndex : Int
    }


type alias DaySelection =
    { i : Int
    , r : Float
    , t : Float
    }


type YearEvent
    = NoChange
    | Changed Day


update : YearViewSettings -> Msg -> YearViewState -> ( YearViewState, Cmd Msg, YearEvent )
update s msg state =
    case msg of
        CurrentDate date ->
            { state | today = date } ! []


initYearState : ( YearViewState, Cmd Msg )
initYearState =
    ( YearViewState 0 0
    , Task.perform CurrentDate Date.now
    )


type alias YearViewSettings msg =
    { diameter : Float
    , toMsg : YearViewState -> msg
    }


dayAngle : Int -> Float
dayAngle year =
    (2.0
        * pi
        / (yearLength year |> toFloat)
    )


yearView : YearViewSettings msg -> YearViewState -> Year -> Html msg
yearView s state year =
    let
        ringWidth =
            100

        diamInt =
            floor s.diameter

        radius =
            s.diameter / 2

        outerRadius =
            radius

        innerRadius =
            outerRadius - ringWidth

        dAngle =
            dayAngle year.year

        outerArcPoint =
            ( outerRadius * (cos dAngle), outerRadius * (sin dAngle) )

        innerArcPoint =
            ( innerRadius * (cos dAngle), innerRadius * (sin dAngle) )

        wedgePath =
            subpath
                (startAt ( innerRadius, 0 ))
                closed
                [ lineTo ( outerRadius, 0 )
                , arcTo ( outerRadius, outerRadius ) 0 ( smallestArc, antiClockwise ) outerArcPoint
                , lineTo innerArcPoint
                , arcTo ( innerRadius, innerRadius ) 0 ( smallestArc, clockwise ) ( innerRadius, 0 )
                ]
    in
        svg
            [ width (px s.diameter)
            , height (px s.diameter)
            , viewBox 0 0 diamInt diamInt
            , shapeRendering RenderGeometricPrecision
            ]
            [ defs []
                [ TypedSvg.path
                    [ HAttrs.id "wedge"
                    , d (pathToString [ wedgePath ])
                    , fill FillNone
                    , strokeWidth (px 0.5)
                    , stroke <| Color.black
                    ]
                    []
                ]
            , g [ transform [ Translate radius radius ] ] <| List.map (dayView s dAngle) year.days
            , rect
                [ x (px 0)
                , y (px 0)
                , width (px s.diameter)
                , height (px s.diameter)
                , fill <| Fill <| Color.rgba 0 0 0 0
                , onMouseMove <| (mouseMove s state year)
                ]
                []

            -- onMouseMove YearMouseAt
            ]


mouseMove : YearViewSettings msg -> YearViewState -> Year -> Point -> msg
mouseMove s state year point =
    let
        { i, r } =
            yearPointToDaySelection s year.year point
    in
        if state.dayIndex /= i then
            s.toMsg { state | dayIndex = i }
        else
            s.toMsg state



-- SetYearViewState << (yearPointToDaySelection s year.year)


yearPointToDaySelection : YearViewSettings msg -> Int -> Point -> DaySelection
yearPointToDaySelection s year { x, y } =
    let
        yr =
            s.diameter / 2

        cx =
            x - yr

        cy =
            y - yr

        ( r, t ) =
            toPolar ( cx, cy )

        tt =
            t + pi

        i =
            floor <| tt / (dayAngle year)
    in
        DaySelection i r tt


dayView : YearViewSettings msg -> Float -> Day -> Html msg
dayView s dAngle day =
    let
        angle =
            180.0 * (dAngle * (toFloat day.index)) / pi
    in
        use [ transform [ Rotate angle 0 0 ], xlinkHref "#wedge" ] []



--, onMouseOver (SelectDay day)
