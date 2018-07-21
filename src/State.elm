module State exposing (..)

import Date exposing (Date)
import Types exposing (..)
import View exposing (yearViewUnmapAngle, yearViewSettings)
import Mouse exposing (Position)


type alias Flags =
    { initialMilliseconds : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model (yearForMilliseconds flags.initialMilliseconds) 0 Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
        case msg of
            SelectDay day ->
                let
                    _ =
                        Debug.log "day" day
                in
                    ( { model | selectedDay = Just day }, Cmd.none )

            YearMove point ->
                let
                    _ =
                        Debug.log "year move" point
                in
                    ( model, Cmd.none )

            _ ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- subscriptions : Model -> Sub Msg
-- subscriptions model =
-- Sub.batch
-- [ Mouse.moves MouseAt
-- , Mouse.ups DragEnd
-- ]
