module View exposing (..)

import Html exposing (..)
import Html.Attributes as HAttrs
import Color
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (Fill(..), Transform(..), ShapeRendering(..), px)
import Types exposing (..)
import Svg.Path exposing (pathToString, subpath, startAt, lineToMany, lineTo, emptySubpath, closed, arcTo, smallestArc, clockwise, antiClockwise)
import View.Mouse exposing (onMouseMove, Point)


-- import ElementRelativeMouseEvents exposing (onMouseMove)


rootView : Model -> Html Msg
rootView model =
    div [ HAttrs.id "app-root" ]
        [ h1 [] [ text <| toString model.year.year ]
        , em [] [ text "hi" ]
        , yearView yearViewSettings model.year
        ]


type alias YearViewSettings =
    { diameter : Float
    }


yearViewSettings =
    { diameter = 800
    }


yearView : YearViewSettings -> Year -> Html Msg
yearView s year =
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

        dayAngle =
            (2.0
                * pi
                / (toFloat (yearLength year.year))
            )

        outerArcPoint =
            ( outerRadius * (cos dayAngle), outerRadius * (sin dayAngle) )

        innerArcPoint =
            ( innerRadius * (cos dayAngle), innerRadius * (sin dayAngle) )

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
            , g [ transform [ Translate radius radius ] ] <| List.map (dayView s dayAngle) year.days
            , rect
                [ x (px 0)
                , y (px 0)
                , width (px s.diameter)
                , height (px s.diameter)
                , fill <| Fill <| Color.rgba 0 0 0 0
                , onMouseMove <| YearMove << (yearPointToPolarPoint s)
                ]
                []

            -- onMouseMove YearMouseAt
            ]


yearPointToPolarPoint : YearViewSettings -> Point -> PolarPoint
yearPointToPolarPoint s { x, y } =
    let
        yr =
            s.diameter / 2

        cx =
            x - yr

        cy =
            y - yr

        ( r, t ) =
            toPolar ( cx, cy )
    in
        PolarPoint r t


unmapYearAngle : YearViewSettings -> ( Float, Float ) -> Float
unmapYearAngle s ( x, y ) =
    let
        _ =
            Debug.log "x" x

        _ =
            Debug.log "y" y
    in
        0.0


yearViewUnmapAngle : YearViewSettings -> { x : Float, y : Float } -> Float
yearViewUnmapAngle s { x, y } =
    let
        _ =
            Debug.log "x" x

        _ =
            Debug.log "y" y
    in
        0.0


dayView : YearViewSettings -> Float -> Day -> Html Msg
dayView s dayAngle day =
    let
        angle =
            180.0 * (dayAngle * (toFloat day.index)) / pi
    in
        use [ transform [ Rotate angle 0 0 ], xlinkHref "#wedge" ] []



--, onMouseOver (SelectDay day)
